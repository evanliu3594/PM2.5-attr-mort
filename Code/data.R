# 数据读取与 RR 查找模块
#   - matchable(), normalize_coords()  — 坐标与数值归一化
#   - read_files()                     — 把所有输入数据读入全局环境
#   - getMortRate(), getConc_real(), getConc_cf(), getPop(), getAgeGroup() — 访问器
#   修改 260618：与类型无关的坐标匹配（any_of + as.numeric），
#     Conc_cf 默认 NULL，domain 列自动归一化（Country→domain）。
#   修改 261026b：坐标不区分大小写，唯一键守卫，严格的
#     Conc_cf 路径，conc_unit 传递，实例清单辅助函数。
#   修改 261102：浓度网格固定为 0.1，删除 dgt_conc 参数。
#   修改 261027：assert_unique_key() 只做一次向量化的 base R 扫描；
#     dplyr count() 管线只在需要构造重复键消息时运行。
#   修改 261101：resolve_instance_files(suffix=) 不再自动带上
#     遗留反事实文件；除非调用方传入 Conc_cf 或清单已登记，
#     Conc_cf 保持 NULL。

# 标准化 RR 查找表（由 set_Model() 通过 RR_std() 构建）
.RR_std_tbl <- NULL

# RR 查找表、Mortality 和输入共用的浓度单位。浓度网格固定为 0.1：
# RR 表的分辨率不得细于 0.1，输入浓度按 0.1 位取整后再连接。
.CONC_UNIT <- NULL
.RR_meta <- list()

# 把数字格式化成按 dgt 位取整的字符串，使连接键按
# 字符串比较。matchable(3.33333, 1) 返回 "3.3"。
matchable <- function(num, dgt = 1) {
  num |> round(dgt) |> str_c()
}

# 把地理坐标列改名为规范的 x（经度）和
# y（纬度）。匹配不区分大小写。经度别名：x、lon、long、
# longitude。纬度别名：y、lat、latitude。某一侧缺失或
# 带多个别名时中止；已规范时原样返回 df。
normalize_coords <- function(df) {
  nms <- names(df)
  lower_nms <- tolower(nms)

  find_coord_col <- function(aliases, label) {
    hits <- which(lower_nms %in% aliases)
    if (length(hits) == 0) {
      return(NA_character_)
    }
    if (length(hits) > 1) {
      stop(
        "Ambiguous ", label, " columns: ",
        paste(nms[hits], collapse = ", "),
        ". Keep exactly one."
      )
    }
    nms[hits]
  }

  lon_col <- find_coord_col(c("longitude", "long", "lon", "x"), "longitude")
  lat_col <- find_coord_col(c("latitude", "lat", "y"), "latitude")

  if (is.na(lon_col) || is.na(lat_col)) {
    found_lon <- nms[lower_nms %in% c("longitude", "long", "lon", "x")]
    found_lat <- nms[lower_nms %in% c("latitude", "lat", "y")]
    stop(
      "Cannot identify geographic coordinate columns.\n",
      "  Found longitude-like columns: ",
      if (length(found_lon) > 0) {
        paste(found_lon, collapse = ", ")
      } else {
        "(none)"
      },
      "\n",
      "  Found latitude-like columns:  ",
      if (length(found_lat) > 0) {
        paste(found_lat, collapse = ", ")
      } else {
        "(none)"
      },
      "\n",
      "  Expected columns named like: x/lon/long/longitude and y/lat/latitude, ",
      "in any letter case."
    )
  }

  if (lon_col != "x" || lat_col != "y") {
    df <- df |>
      rename(x = all_of(lon_col), y = all_of(lat_col))
    log_msg(
      INFO,
      "Coordinates normalized: \"{lon_col}\" -> \"x\", \"{lat_col}\" -> \"y\""
    )
  }

  df
}

# 守卫：断言 df 的连接键唯一且非缺失。keys 保存键
# 列；label 作为错误消息前缀。列缺失、键为 NA
# 或键重复时中止。以不可见方式返回 TRUE。
assert_unique_key <- function(df, keys, label) {
  missing_cols <- setdiff(keys, names(df))
  if (length(missing_cols) > 0) {
    stop(
      label, " is missing key column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

# 向量化守卫：对键列做一次 base R 扫描。dplyr 管线
# 只在确实有问题时运行，用于构造诊断消息。
  key_df <- df[keys]
  na_rows <- sum(!stats::complete.cases(key_df))
  if (na_rows > 0) {
    stop(label, " has ", na_rows, " row(s) with NA in key columns.")
  }

  if (anyDuplicated(key_df) == 0L) {
    return(invisible(TRUE))
  }

  duplicates <- df |>
    count(across(all_of(keys)), name = "n") |>
    filter(n > 1)
  if (nrow(duplicates) > 0) {
    examples <- duplicates |>
      head(3) |>
      unite("key", all_of(keys), sep = "/") |>
      pull(key)
    stop(
      label, " has ", nrow(duplicates), " duplicated key combination(s) on ",
      paste(keys, collapse = "/"), ". Examples: ",
      paste(examples, collapse = ", "),
      ". Aggregate or deduplicate the input before reading."
    )
  }

  invisible(TRUE)
}

# 把所有输入数据读入全局环境。读取网格文件
# （Grid_info、Pop、Conc_real、Conc_cf）和域级文件（MortRate、
# AgeGroup），来源为 xlsx 或 csv；归一化坐标列名与 domain 列名，
# 把连接键转成可匹配字符串，并在全局赋值这些数据框。
#   Grids, Pop, Conc_real, MortRate, AgeGroup  输入路径。
#   Conc_cf    反事实路径；NULL 时回退到 Conc_real。
#   dgt_grid   坐标精度，默认 2。
#   conc_unit  可选单位，如 "ug/m3" 或 "ppb"；必须与当前 RR
#              表元数据一致。
# 浓度网格固定为 0.1：输入浓度按 0.1 位取整，不再暴露精度参数。
# 无返回值：唯一输出是全局赋值。
read_files <- function(
  Grids,
  Pop,
  Conc_real,
  Conc_cf = NULL,
  MortRate,
  AgeGroup,
  dgt_grid = 2,
  conc_unit = NULL
) {
  if (length(dgt_grid) != 1 || dgt_grid < 0 || dgt_grid != as.integer(dgt_grid)) {
    stop("dgt_grid must be one non-negative integer.")
  }

  if (!is.null(conc_unit)) {
    if (length(conc_unit) != 1 || !nzchar(conc_unit)) {
      stop("conc_unit must be one non-empty string when supplied.")
    }
    rr_unit <- .RR_meta$conc_unit %||% NULL
    if (!is.null(rr_unit) && !identical(rr_unit, conc_unit)) {
      stop(
        "Concentration unit mismatch: model expects \"", rr_unit,
        "\", read_files() received \"", conc_unit, "\"."
      )
    }
  }

#  守卫：文件是否存在
  if (!is.null(Conc_cf) && !file.exists(Conc_cf)) {
    stop(
      "Conc_cf file not found: \"", Conc_cf,
      "\". Supply an existing file or omit Conc_cf explicitly."
    )
  }
  required_files <- list(
    Grids = Grids,
    Pop = Pop,
    Conc_real = Conc_real,
    MortRate = MortRate,
    AgeGroup = AgeGroup
  )
  if (!is.null(Conc_cf)) {
    required_files[["Conc_cf"]] <- Conc_cf
  }
  for (f in required_files) {
    if (!file.exists(f)) {
      stop(
        "File not found: \"",
        f,
        "\". Check that the path is correct and the file exists."
      )
    }
  }

# domain 列可接受的列名变体（下面归一化为 "domain"）
  domain_candidates <- c("Country", "country", "location", "Location")

  fuse_read <- function(filename) {
    if (str_detect(filename, 'csv$')) {
      read_csv(filename)
    } else if (str_detect(filename, 'xlsx$')) {
      read_xlsx(filename)
    }
  }

  grid_df <- fuse_read(Grids) |>
    normalize_coords() |>
    mutate(across(any_of(c("x", "y")), ~ matchable(as.numeric(.x), dgt = dgt_grid)))

  if (!all(c("x", "y") %in% names(grid_df))) {
    stop(
      "Grid_info must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(grid_df), collapse = ", ")
    )
  }
  assert_unique_key(grid_df, c("x", "y"), "Grid_info after coordinate rounding")

  assign('Grid_info', envir = globalenv(), grid_df)

  pop_df <- fuse_read(Pop) |>
    normalize_coords() |>
    mutate(across(any_of(c("x", "y")), ~ matchable(as.numeric(.x), dgt = dgt_grid)))

  if (!all(c("x", "y") %in% names(pop_df))) {
    stop(
      "Pop data must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(pop_df), collapse = ", ")
    )
  }
  assert_unique_key(pop_df, c("x", "y"), "Pop after coordinate rounding")

  assign("Pop", envir = globalenv(), pop_df)

  conc_real_df <- fuse_read(Conc_real) |>
    normalize_coords() |>
    mutate(
      across(any_of(c("x", "y")), ~ matchable(as.numeric(.x), dgt = dgt_grid)),
      across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = 1))
    )

  if (!all(c("x", "y") %in% names(conc_real_df))) {
    stop(
      "Conc_real must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(conc_real_df), collapse = ", ")
    )
  }
  assert_unique_key(conc_real_df, c("x", "y"), "Conc_real after coordinate rounding")

  assign('Conc_real', envir = globalenv(), conc_real_df)

  if (!is.null(Conc_cf)) {
    if (!file.exists(Conc_cf)) {
      stop(
        "Conc_cf file not found: \"", Conc_cf,
        "\". Supply an existing file or omit Conc_cf explicitly."
      )
    }
    conc_cf_df <- fuse_read(Conc_cf) |>
      normalize_coords() |>
      mutate(
        across(any_of(c("x", "y")), ~ matchable(as.numeric(.x), dgt = dgt_grid)),
        across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = 1))
      )
    if (!all(c("x", "y") %in% names(conc_cf_df))) {
      stop(
        "Conc_cf must contain geographic coordinate columns (x/lon/long, y/lat). ",
        "Found: ",
        paste(names(conc_cf_df), collapse = ", ")
      )
    }
    assert_unique_key(conc_cf_df, c("x", "y"), "Conc_cf after coordinate rounding")
  } else {
    conc_cf_df <- NULL
    log_msg(INFO, "Conc_cf not supplied; counterfactual data are unavailable.")
  }

  assign('Conc_cf', envir = globalenv(), conc_cf_df)

  mort_rate_raw <- fuse_read(MortRate)

# 归一化 domain 列名（见上面的 domain_candidates）
  found_domain <- intersect(domain_candidates, names(mort_rate_raw))[1]
  if (!is.na(found_domain) && !"domain" %in% names(mort_rate_raw)) {
    mort_rate_raw <- mort_rate_raw |> rename(domain = !!found_domain)
  }

  if (!all(c("domain", "endpoint", "agegroup") %in% names(mort_rate_raw))) {
    stop(
      "MortRate must contain columns \"domain\", \"endpoint\", \"agegroup\". Found: ",
      paste(names(mort_rate_raw), collapse = ", ")
    )
  }

  mort_rate_df <- mort_rate_raw |>
    mutate(
      endpoint = tolower(endpoint),
      across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0))
    )
  assert_unique_key(
    mort_rate_df,
    c("domain", "endpoint", "agegroup"),
    "MortRate"
  )
  assign('MortRate', envir = globalenv(), mort_rate_df)

  age_group_raw <- fuse_read(AgeGroup)

# 归一化 domain 列名（与上面 MortRate 的逻辑相同）
  found_domain_ag <- intersect(domain_candidates, names(age_group_raw))[1]
  if (!is.na(found_domain_ag) && !"domain" %in% names(age_group_raw)) {
    age_group_raw <- age_group_raw |> rename(domain = !!found_domain_ag)
  }

  if (!all(c("domain", "agegroup") %in% names(age_group_raw))) {
    stop(
      "AgeGroup must contain columns \"domain\", \"agegroup\". Found: ",
      paste(names(age_group_raw), collapse = ", ")
    )
  }

  age_group_df <- age_group_raw |>
    mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  assert_unique_key(age_group_df, c("domain", "agegroup"), "AgeGroup")
  assign("AgeGroup", envir = globalenv(), age_group_df)

  assign(".CONC_UNIT", conc_unit, envir = globalenv())

#  守卫：.CR_Model 必须已设置
  if (!exists('.CR_Model', envir = globalenv())) {
    stop(
      "C-R model not set. Call set_Model() before read_files(). ",
      "Supported models: IER, IER2010–IER2017, NCD+LRI, 5COD, MRBRT, O3, NO2."
    )
  }

#  守卫：坐标一致性摘要
  n_grid <- nrow(grid_df)
  n_conc <- nrow(conc_real_df)
  n_pop <- nrow(pop_df)
  overlap_gc <- grid_df |>
    select(x, y) |>
    inner_join(conc_real_df |> select(x, y), by = c("x", "y")) |>
    nrow()
  overlap_gp <- grid_df |>
    select(x, y) |>
    inner_join(pop_df |> select(x, y), by = c("x", "y")) |>
    nrow()

  log_msg(
    INFO,
    "Data loaded: {n_grid} grids, {n_conc} concentration records, {n_pop} population records."
  )
  log_msg(
    INFO,
    "  Grids × Conc overlap: {overlap_gc}/{n_grid} ({round(overlap_gc/min(n_grid,n_conc)*100)}% of smaller)"
  )
  log_msg(
    INFO,
    "  Grids × Pop  overlap: {overlap_gp}/{n_grid} ({round(overlap_gp/min(n_grid,n_pop)*100)}% of smaller)"
  )

# 仅当从较小数据集的视角看重叠率偏低时才告警：
# 网格文件覆盖范围大于 Conc/Pop 属正常情况（多出的网格
# 没有数据，直接丢弃）。
  conc_overlap_pct <- overlap_gc / min(n_grid, n_conc)
  pop_overlap_pct <- overlap_gp / min(n_grid, n_pop)

  if (conc_overlap_pct < 0.8 || pop_overlap_pct < 0.8) {
    log_msg(
      WARN,
      "Low coordinate overlap between Grid_info and Conc/Pop data ",
      "(Conc: {round(conc_overlap_pct * 100)}%, ",
      "Pop: {round(pop_overlap_pct * 100)}% of smaller). ",
      "Check that all files use the same (x, y) precision (dgt_grid = {dgt_grid}) ",
      "and cover the same geographic domain."
    )
  }
}

# MortRate 访问器：取一个年份/情景列作为 MortRate，并把
# endpoint 转为小写。返回 domain、endpoint、agegroup、MortRate。
# MortRate 未加载或 at 不是列时中止。
getMortRate <- function(at) {
#  守卫：MortRate 已加载
  if (!exists('MortRate', envir = globalenv())) {
    stop("MortRate not found in global environment. Run read_files() first.")
  }
#  守卫：年份列存在
  if (!at %in% names(MortRate)) {
    stop(
      "Year/scenario \"",
      at,
      "\" not found in MortRate columns. ",
      "Available: ",
      paste(
        setdiff(names(MortRate), c("domain", "endpoint", "agegroup")),
        collapse = ", "
      )
    )
  }

  # fmt: skip
  MortRate |>
    mutate(endpoint = tolower(endpoint)) |>
    select(domain, endpoint, agegroup, MortRate = {at})
}

# Conc_real 访问器：取一个年份/情景列作为浓度。返回
# x、y、concentration。Conc_real 未加载或 at 不是列时中止。
getConc_real <- function(at) {
  if (!exists('Conc_real', envir = globalenv())) {
    stop("Conc_real not found in global environment. Run read_files() first.")
  }
  if (!at %in% names(Conc_real)) {
    stop(
      "Year/scenario \"",
      at,
      "\" not found in Conc_real columns. ",
      "Available: ",
      paste(setdiff(names(Conc_real), c("x", "y")), collapse = ", ")
    )
  }
  # fmt: skip
  Conc_real |> select(x, y, concentration = {at})
}

# Conc_cf 访问器：取一个年份/情景列作为浓度。返回
# x、y、concentration。Conc_cf 未加载或为 NULL，
# 或 at 不是列时中止。
getConc_cf <- function(at) {
  if (!exists('Conc_cf', envir = globalenv()) || is.null(Conc_cf)) {
    stop(
      "Conc_cf not found in global environment (it was NULL or not loaded). ",
      "Ensure the Conc_cf file exists and was loaded by read_files()."
    )
  }
  if (!at %in% names(Conc_cf)) {
    stop(
      "Year/scenario \"",
      at,
      "\" not found in Conc_cf columns. ",
      "Available: ",
      paste(setdiff(names(Conc_cf), c("x", "y")), collapse = ", ")
    )
  }

  # fmt: skip
  Conc_cf |> select(x, y, concentration = {at})
}

# Pop 访问器：取一个年份/情景列作为 Pop。返回 x、y、Pop。
# Pop 未加载或 at 不是列时中止。
getPop <- function(at) {
  if (!exists('Pop', envir = globalenv())) {
    stop("Pop not found in global environment. Run read_files() first.")
  }
  if (!at %in% names(Pop)) {
    stop(
      "Year/scenario \"",
      at,
      "\" not found in Pop columns. ",
      "Available: ",
      paste(setdiff(names(Pop), c("x", "y")), collapse = ", ")
    )
  }
  # fmt: skip
  Pop |> select(x, y, Pop = {at})
}

# AgeGroup 访问器：取一个年份/情景列作为 AgeStruc。返回 domain、
# agegroup、AgeStruc。AgeGroup 未加载或 at 不是列时中止。
getAgeGroup <- function(at) {
  if (!exists('AgeGroup', envir = globalenv())) {
    stop("AgeGroup not found in global environment. Run read_files() first.")
  }
  if (!at %in% names(AgeGroup)) {
    stop(
      "Year/scenario \"",
      at,
      "\" not found in AgeGroup columns. ",
      "Available: ",
      paste(setdiff(names(AgeGroup), c("domain", "agegroup")), collapse = ", ")
    )
  }

  # fmt: skip
  AgeGroup |> select(domain, agegroup, AgeStruc = {at})
}

# 列出网格数据框的情景列：除 x 和
# y 之外的每一列。df 默认取全局 Conc_real。df 未加载或
# 没有情景列时中止。
get_scenarios <- function(df = NULL) {
  if (is.null(df)) {
    if (!exists("Conc_real", envir = globalenv())) {
      stop("Conc_real not found. Run read_files() first or supply df.")
    }
    df <- Conc_real
  }
  scenarios <- setdiff(names(df), c("x", "y"))
  if (length(scenarios) == 0) {
    stop("No scenario columns found outside x and y.")
  }
  scenarios
}

# 解析实例输入路径，来源可以是构建清单，也可以是
# 实例后缀。返回命名列表，含 Grids、Pop、Conc_real、Conc_cf、
# MortRate 和 AgeGroup。
#   manifest_path  Data/instance_manifest.json 的路径。
#   suffix         可选的六位实例后缀；给出该参数时忽略清单，
#                  改用标准路径构造。
#                  反事实文件永不自动带上：Conc_cf 保持 NULL，
#                  调用方必须显式传入经批准的路径。
# 只有清单登记了 Conc_cf 时才填充该项。后缀非法、
# 清单或条目缺失，或文件缺失时中止。
resolve_instance_files <- function(
  manifest_path = "./Data/instance_manifest.json",
  suffix = NULL
) {
  if (!is.null(suffix)) {
    if (!str_detect(suffix, "^[0-9]{6}$")) {
      stop("suffix must contain exactly six digits, for example 260617.")
    }
    files <- list(
      Grids = str_glue("./Data/Grid_info_instance_{suffix}.xlsx"),
      Pop = str_glue("./Data/GridPop_instance_{suffix}.xlsx"),
      Conc_real = str_glue("./Data/GridPM25_instance_{suffix}.xlsx"),
# 遗留的 GridPM25_cf_instance_*.xlsx 已记录为不可用
# （浓度为负），因此永不自动选用。
      Conc_cf = NULL,
      MortRate = str_glue("./Data/GBD_mortality_instance_{suffix}.xlsx"),
      AgeGroup = str_glue("./Data/GBD_agestructure_instance_{suffix}.xlsx")
    )
    log_msg(
      INFO,
      "Conc_cf is not auto-included for instance {suffix}; ",
      "pass an approved counterfactual file explicitly."
    )
  } else {
    if (!file.exists(manifest_path)) {
      stop(
        "Instance manifest not found: ", manifest_path,
        ". Run build_instance.R or pass suffix explicitly."
      )
    }
    manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
    files <- as.list(unlist(manifest$files, use.names = TRUE))
    required <- c("Grids", "Pop", "Conc_real", "MortRate", "AgeGroup")
    missing_entries <- setdiff(required, names(files))
    if (length(missing_entries) > 0) {
      stop(
        "Instance manifest is missing: ",
        paste(missing_entries, collapse = ", ")
      )
    }
    if (!"Conc_cf" %in% names(files)) {
      files$Conc_cf <- NULL
    }
  }

  missing_files <- files |>
    unlist(use.names = FALSE) |>
    keep(~ !is.null(.x) && !file.exists(.x))
  if (length(missing_files) > 0) {
    stop(
      "Instance file(s) not found: ",
      paste(missing_files, collapse = ", ")
    )
  }

  files
}
