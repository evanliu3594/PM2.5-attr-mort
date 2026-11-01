# Data loading & RR lookup module
#   - matchable(), normalize_coords()  — coordinate & numeric normalisation
#   - read_files()                     — load all input data to global env
#   - getMortRate(), getConc_real(), getConc_cf(), getPop(), getAgeGroup() — accessors
#   Modified 260618: type-agnostic coordinate matching (any_of + as.numeric),
#     Conc_cf defaults to NULL, domain column auto-normalisation (Country→domain).
#   Modified 261026b: case-insensitive coordinates, unique-key guards, strict
#     Conc_cf paths, dgt_conc/conc_unit propagation, instance manifest helpers.
#   Modified 261027: assert_unique_key() performs one vectorised base R scan;
#     the dplyr count() pipeline runs only to build the duplicate-key message.
#   Modified 261101: resolve_instance_files(suffix=) no longer auto-includes the
#     legacy counterfactual file; Conc_cf stays NULL unless the caller passes it
#     or the manifest registers it.

# Standardised RR lookup table (built by set_Model() via RR_std())
.RR_std_tbl <- NULL

# Concentration precision and unit shared by RR lookup, Mortality, and inputs
.DGT_CONC <- 1
.CONC_UNIT <- NULL
.RR_meta <- list()

# Format a number to a string rounded at dgt digits, so join keys compare as
# strings. matchable(3.33333, 1) returns "3.3".
matchable <- function(num, dgt = 1) {
  num |> round(dgt) |> str_c()
}

# Rename geographic coordinate columns to the canonical x (longitude) and
# y (latitude). Matching ignores letter case. Longitude aliases: x, lon, long,
# longitude. Latitude aliases: y, lat, latitude. Aborts when a side is absent or
# carries more than one alias; returns df unchanged when already canonical.
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

# Guard: assert that df has unique, non-missing join keys. keys holds the key
# columns; label prefixes the error message. Aborts on a missing column, an NA
# key, or a duplicated key. Returns TRUE invisibly.
assert_unique_key <- function(df, keys, label) {
  missing_cols <- setdiff(keys, names(df))
  if (length(missing_cols) > 0) {
    stop(
      label, " is missing key column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Vectorised guard: one base R pass over the key columns. The dplyr pipeline
  # below runs only when a problem exists, to build the diagnostic message.
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

# Load all input data into the global environment. Reads the gridded files
# (Grid_info, Pop, Conc_real, Conc_cf) and the domain-level files (MortRate,
# AgeGroup) from xlsx or csv, normalises coordinate and domain column names,
# converts join keys to matchable strings, and assigns the frames globally.
#   Grids, Pop, Conc_real, MortRate, AgeGroup  input paths.
#   Conc_cf    counterfactual path; NULL falls back to Conc_real.
#   dgt_grid   coordinate precision, default 2.
#   dgt_conc   concentration precision, default 1; must equal the precision the
#              active RR table was built at.
#   conc_unit  optional unit such as "ug/m3" or "ppb"; must match the active RR
#              table metadata.
# Returns nothing: the only output is the global assignment.
read_files <- function(
  Grids,
  Pop,
  Conc_real,
  Conc_cf = NULL,
  MortRate,
  AgeGroup,
  dgt_grid = 2,
  dgt_conc = 1,
  conc_unit = NULL
) {
  if (length(dgt_grid) != 1 || dgt_grid < 0 || dgt_grid != as.integer(dgt_grid)) {
    stop("dgt_grid must be one non-negative integer.")
  }
  if (length(dgt_conc) != 1 || dgt_conc < 0 || dgt_conc != as.integer(dgt_conc)) {
    stop("dgt_conc must be one non-negative integer.")
  }

  rr_dgt <- attr(.RR_std_tbl, "dgt_conc", exact = TRUE)
  if (!is.null(.RR_std_tbl) && !is.null(rr_dgt) && rr_dgt != dgt_conc) {
    stop(
      "The active RR table was built at dgt_conc = ", rr_dgt,
      ", but read_files() requested dgt_conc = ", dgt_conc,
      ". Call set_Model(..., dgt_conc = ", dgt_conc, ") first."
    )
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

  #  Guard: file existence
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

  # Accepted column name variants for domain (normalized to "domain" below)
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
      across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
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
        across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
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

  # Normalize domain column name (see domain_candidates above)
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

  # Normalize domain column name (same logic as MortRate above)
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

  assign(".DGT_CONC", dgt_conc, envir = globalenv())
  assign(".CONC_UNIT", conc_unit, envir = globalenv())

  #  Guard: .CR_Model must be set
  if (!exists('.CR_Model', envir = globalenv())) {
    stop(
      "C-R model not set. Call set_Model() before read_files(). ",
      "Supported models: IER, IER2010–IER2017, NCD+LRI, 5COD, MRBRT, O3, NO2."
    )
  }

  #  Guard: coordinate consistency summary
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

  # Warn only if overlap is low from the perspective of the SMALLER dataset —
  # a grid file covering a larger region than Conc/Pop is normal (extra grids
  # without data are simply dropped).
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

# MortRate accessor: pick one year/scenario column as MortRate and lower-case
# endpoint. Returns domain, endpoint, agegroup, MortRate. Aborts when MortRate is
# not loaded or at is not a column.
getMortRate <- function(at) {
  #  Guard: MortRate loaded
  if (!exists('MortRate', envir = globalenv())) {
    stop("MortRate not found in global environment. Run read_files() first.")
  }
  #  Guard: year column exists
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

# Conc_real accessor: pick one year/scenario column as concentration. Returns
# x, y, concentration. Aborts when Conc_real is not loaded or at is not a column.
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

# Conc_cf accessor: pick one year/scenario column as concentration. Returns
# x, y, concentration. Aborts when Conc_cf is not loaded or was NULL, or when at
# is not a column.
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

# Pop accessor: pick one year/scenario column as Pop. Returns x, y, Pop. Aborts
# when Pop is not loaded or at is not a column.
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

# AgeGroup accessor: pick one year/scenario column as AgeStruc. Returns domain,
# agegroup, AgeStruc. Aborts when AgeGroup is not loaded or at is not a column.
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

# List the scenario columns of a gridded data frame: every column except x and
# y. df defaults to the global Conc_real. Aborts when df is not loaded or has no
# scenario column.
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

# Resolve the instance input paths, either from the build manifest or from an
# instance suffix. Returns a named list with Grids, Pop, Conc_real, Conc_cf,
# MortRate, and AgeGroup.
#   manifest_path  path to Data/instance_manifest.json.
#   suffix         optional six-digit instance suffix; when supplied the manifest
#                  is ignored and standard paths are built. The counterfactual
#                  file is never auto-included: Conc_cf stays NULL and callers
#                  must pass an approved path explicitly.
# Conc_cf is populated only when the manifest registers it. Aborts on a bad
# suffix, a missing manifest or entry, or a missing file.
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
      # The legacy GridPM25_cf_instance_*.xlsx is documented as unusable
      # (negative concentrations), so it is never picked up automatically.
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
