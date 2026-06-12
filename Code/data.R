# Data loading & RR lookup module
#   - matchable(), normalize_coords()  — coordinate & numeric normalisation
#   - read_files()                     — load all input data to global env
#   - RR_std()                         — reshape CR lookup table
#   - getMortRate(), getConc_real(), getConc_cf(), getPop(), getAgeGroup() — accessors
#   Modified 260610-260613: concentration clamping, auto-detect PWRR domain,
#     coordinate overlap checks, GEMM lookup table update, fix scoping issues.

#' format numbers to a string at a specified digit
#'
#' @param num input number
#' @param dgt int, refers to rounding digit
#'
#' @return string of number
#' @export
#'
#' @examples
#' matchable(3.33333, 1)
matchable <- function(num, dgt = 1) {
  num %>% round(dgt) %>% str_c
}

#' Normalize geographic coordinate column names to "x" and "y"
#'
#' Detects common longitude/latitude column name variants (case-insensitive)
#' and renames them to the canonical `x` (longitude) and `y` (latitude).
#'
#' Recognized longitude names: x, lon, long, longitude
#' Recognized latitude names:  y, lat, latitude
#'
#' @param df a data frame
#' @return the data frame with coordinate columns renamed to `x` and `y`
#' @keywords internal
normalize_coords <- function(df) {
  nms <- names(df)

  # Longitude candidates (order: most specific first to avoid partial matches)
  lon_candidates <- c(
    "longitude",
    "Longitude",
    "LONGITUDE",
    "long",
    "Long",
    "LONG",
    "lon",
    "Lon",
    "LON",
    "x",
    "X"
  )

  # Latitude candidates
  lat_candidates <- c(
    "latitude",
    "Latitude",
    "LATITUDE",
    "lat",
    "Lat",
    "LAT",
    "y",
    "Y"
  )

  lon_col <- intersect(lon_candidates, nms)[1]
  lat_col <- intersect(lat_candidates, nms)[1]

  if (is.na(lon_col) || is.na(lat_col)) {
    found_lon <- intersect(lon_candidates, nms)
    found_lat <- intersect(lat_candidates, nms)
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
      "  Expected columns named like: x/lon/long/longitude and y/lat/latitude."
    )
  }

  if (lon_col != "x" || lat_col != "y") {
    df <- df %>% rename(x = !!lon_col, y = !!lat_col)
    cat(str_glue(
      "  Coordinates normalized: \"{lon_col}\" -> \"x\", \"{lat_col}\" -> \"y\"\n"
    ))
  }

  return(df)
}

#' dataload module for calculation
#'
#' @param Grids path to grid information
#' @param Pop path to population
#' @param Conc_real path to real concentration
#' @param Conc_cf path to counter-fact concentration
#' @param MortRate path to mortality rate
#' @param AgeGroup path to population age structure
#' @param dgt_grid int, digit of map grids
#' @param dgt_conc int, digit of concentrations, by default `1`
#'
#' @return assign data to global env.
#' @export
#'
#' @examples
read_files <- function(
  Grids,
  Pop,
  Conc_real,
  Conc_cf,
  MortRate,
  AgeGroup,
  dgt_grid = 2,
  dgt_conc = 1
) {
  # ---- Guard: file existence ----
  for (f in list(
    Grids = Grids,
    Pop = Pop,
    Conc_real = Conc_real,
    MortRate = MortRate,
    AgeGroup = AgeGroup
  )) {
    if (!file.exists(f)) {
      stop(
        "File not found: \"",
        f,
        "\". Check that the path is correct and the file exists."
      )
    }
  }

  fuse_read <- function(filename) {
    if (str_detect(filename, 'csv$')) {
      read_csv(filename)
    } else if (str_detect(filename, 'xlsx$')) {
      read_xlsx(filename)
    }
  }

  grid_df <- fuse_read(Grids) %>%
    normalize_coords() %>%
    mutate(across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)))

  if (!all(c("x", "y") %in% names(grid_df))) {
    stop(
      "Grid_info must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(grid_df), collapse = ", ")
    )
  }

  assign('Grid_info', envir = globalenv(), grid_df)

  pop_df <- fuse_read(Pop) %>%
    normalize_coords() %>%
    mutate(across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)))

  if (!all(c("x", "y") %in% names(pop_df))) {
    stop(
      "Pop data must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(pop_df), collapse = ", ")
    )
  }

  assign("Pop", envir = globalenv(), pop_df)

  conc_real_df <- fuse_read(Conc_real) %>%
    normalize_coords() %>%
    mutate(
      across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)),
      across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
    )

  if (!all(c("x", "y") %in% names(conc_real_df))) {
    stop(
      "Conc_real must contain geographic coordinate columns (x/lon/long, y/lat). ",
      "Found: ",
      paste(names(conc_real_df), collapse = ", ")
    )
  }

  assign('Conc_real', envir = globalenv(), conc_real_df)

  # Specify UNREAL PM2.5 data, used for only counter-fact scenario.

  if (file.exists(Conc_cf)) {
    conc_cf_df <- fuse_read(Conc_cf) %>%
      normalize_coords() %>%
      mutate(
        across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)),
        across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
      )
    if (!all(c("x", "y") %in% names(conc_cf_df))) {
      stop(
        "Conc_cf must contain geographic coordinate columns (x/lon/long, y/lat). ",
        "Found: ",
        paste(names(conc_cf_df), collapse = ", ")
      )
    }
  } else {
    conc_cf_df <- NULL
  }

  assign('Conc_cf', envir = globalenv(), conc_cf_df)

  if (is.null(conc_cf_df)) {
    warning(
      "Conc_cf file \"",
      Conc_cf,
      "\" not found — Conc_cf set to NULL. ",
      "Counterfactual scenarios will fall back to real concentrations."
    )
  }

  mort_rate_raw <- fuse_read(MortRate)
  if (!all(c("domain", "endpoint", "agegroup") %in% names(mort_rate_raw))) {
    stop(
      "MortRate must contain columns \"domain\", \"endpoint\", \"agegroup\". Found: ",
      paste(names(mort_rate_raw), collapse = ", ")
    )
  }

  assign(
    'MortRate',
    envir = globalenv(),
    mort_rate_raw %>%
      mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  )

  age_group_raw <- fuse_read(AgeGroup)
  if (!all(c("domain", "agegroup") %in% names(age_group_raw))) {
    stop(
      "AgeGroup must contain columns \"domain\", \"agegroup\". Found: ",
      paste(names(age_group_raw), collapse = ", ")
    )
  }

  assign(
    "AgeGroup",
    envir = globalenv(),
    age_group_raw %>%
      mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  )

  # ---- Guard: .CR_Model must be set ----
  if (!exists('.CR_Model', envir = globalenv())) {
    stop(
      "C-R model not set. Call set_Model() before read_files(). ",
      "Supported models: IER, IER2010–IER2017, NCD+LRI, 5COD, MRBRT, O3, NO2."
    )
  }

  CR_file <- if (.CR_Model == 'MRBRT') {
    './Data/RR_index/MRBRT2019_Lookup_Table_LYF220601.xlsx'
  } else if (.CR_Model %in% c('NCD+LRI', '5COD')) {
    './Data/RR_index/GEMM_Lookup_Table_Build_220914.xlsx'
  } else if (.CR_Model %in% c('IER', 'IER2017')) {
    './Data/RR_index/IER2017_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2015') {
    './Data/RR_index/IER2015_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2013') {
    './Data/RR_index/IER2013_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2010') {
    './Data/RR_index/IER2010_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == "O3") {
    "./Data/RR_index/O3_CR_Lookup_Table.xlsx"
  } else if (.CR_Model == "NO2") {
    "./Data/RR_index/NO2_CR_Lookup_Table.xlsx"
  } else {
    stop(
      "Unknown C-R model \"",
      .CR_Model,
      "\". ",
      "Supported: IER, IER2010–IER2017, NCD+LRI, 5COD, MRBRT, O3, NO2."
    )
  }

  if (!file.exists(CR_file)) {
    stop(
      "RR lookup table file not found: \"",
      CR_file,
      "\". ",
      "Check that the file exists and the C-R model name is correct."
    )
  }

  assign(
    "RR_table",
    envir = globalenv(),
    expand_grid(excel_sheets(CR_file), CR_file) %>%
      deframe %>%
      imap(
        ~ read_excel(.x, sheet = .y) %>%
          mutate(
            across(
              where(is.numeric) & concentration,
              ~ matchable(.x, dgt = dgt_conc)
            )
          )
      )
  )

  # ---- Guard: RR_table has expected sheets ----
  expected_sheets <- c("MEAN", "LOW", "UP")
  missing_sheets <- setdiff(expected_sheets, names(RR_table))
  if (length(missing_sheets) > 0) {
    warning(
      "RR lookup table is missing expected sheet(s): ",
      paste(missing_sheets, collapse = ", "),
      ". ",
      "Available: ",
      paste(names(RR_table), collapse = ", "),
      ". ",
      "Uncertainty calculation will fail if these are needed."
    )
  }

  # ---- Guard: coordinate consistency summary ----
  n_grid <- nrow(grid_df)
  n_conc <- nrow(conc_real_df)
  n_pop <- nrow(pop_df)
  overlap_gc <- grid_df %>%
    select(x, y) %>%
    inner_join(conc_real_df %>% select(x, y), by = c("x", "y")) %>%
    nrow
  overlap_gp <- grid_df %>%
    select(x, y) %>%
    inner_join(pop_df %>% select(x, y), by = c("x", "y")) %>%
    nrow

  cat(str_glue(
    "Data loaded: {n_grid} grids, {n_conc} concentration records, {n_pop} population records.\n",
    "  Grids × Conc overlap: {overlap_gc}/{n_grid}  ({round(overlap_gc/min(n_grid,n_conc)*100)}% of smaller)\n",
    "  Grids × Pop  overlap: {overlap_gp}/{n_grid}  ({round(overlap_gp/min(n_grid,n_pop)*100)}% of smaller)\n"
  ))

  # Warn only if overlap is low from the perspective of the SMALLER dataset —
  # a grid file covering a larger region than Conc/Pop is normal (extra grids
  # without data are simply dropped).
  conc_overlap_pct <- overlap_gc / min(n_grid, n_conc)
  pop_overlap_pct <- overlap_gp / min(n_grid, n_pop)

  if (conc_overlap_pct < 0.8 || pop_overlap_pct < 0.8) {
    warning(
      "Low coordinate overlap between Grid_info and Conc/Pop data ",
      "(Conc: ",
      round(conc_overlap_pct * 100),
      "%, Pop: ",
      round(pop_overlap_pct * 100),
      "% of smaller). ",
      "Check that all files use the same (x, y) precision (dgt_grid = ",
      dgt_grid,
      ") and cover the same geographic domain."
    )
  }
}

#' format CR look-up table
#'
#' @param RR_index string, specifying whitch rr table to use, by default the "MEAN" RR
#'
#' @return a formatted RR table
#' @export
#'
#' @examples
RR_std <- function(RR_index = "MEAN") {
  # ---- Guard: CR model must be set ----
  if (!exists('.CR_Model', envir = globalenv())) {
    stop("C-R model not set. Call set_Model() before using RR_std().")
  }

  CR <- tryCatch(
    get(".CR_Model", envir = globalenv()),
    error = function(e) stop("Cannot access .CR_Model from global environment.")
  )

  # ---- Guard: RR_table is loaded ----
  if (!exists('RR_table', envir = globalenv())) {
    stop("RR_table not found in global environment. Run read_files() first.")
  }

  if (!is.list(RR_table)) {
    stop(
      "RR_table is not a list (found: ",
      class(RR_table),
      "). Run read_files() to reload. If using a custom RR function, ",
      "set .CR_Model to a supported model or provide a manually constructed RR_table."
    )
  }

  # ---- Guard: requested RR_index exists ----
  if (!RR_index %in% names(RR_table)) {
    stop(
      "RR_index \"",
      RR_index,
      "\" not found in RR_table. ",
      "Available indices: ",
      paste(names(RR_table), collapse = ", ")
    )
  }

  RR_tbl <- RR_table[[RR_index]] %>%
    pivot_longer(
      cols = -concentration,
      values_to = "RR",
      names_to = c("endpoint", "agegroup"),
      names_sep = '_'
    ) %>%
    mutate(endpoint = tolower(endpoint))

  # ---- Guard: RR_tbl has data after pivot ----
  if (nrow(RR_tbl) == 0) {
    stop(
      "RR_table[[\"",
      RR_index,
      "\"]] is empty after pivot_longer. ",
      "Check that the sheet has columns beyond 'concentration' with names like 'copd_25'."
    )
  }

  RR_reshape <- if (CR == '5COD') {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      group_by(concentration, endpoint) %>%
      fill(RR) %>%
      ungroup %>%
      filter(agegroup != 'ALL')
  } else if (CR == 'NCD+LRI') {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = c('ncd+lri'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      group_by(concentration, endpoint) %>%
      fill(RR) %>%
      ungroup %>%
      filter(agegroup != 'ALL')
  } else if (str_detect(CR, 'IER')) {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'stroke', 'lri'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      group_by(concentration, endpoint) %>%
      fill(RR) %>%
      filter(agegroup != 'ALL') %>%
      filter(
        (endpoint %>%
          str_detect('copd|ihd|lc|stroke') &
          as.integer(agegroup) >= 25) |
          endpoint == 'lri'
      ) %>%
      ungroup
  } else if (CR == 'MRBRT') {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = c('copd', 'dm', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      group_by(concentration, endpoint) %>%
      fill(RR) %>%
      filter(agegroup != 'ALL') %>%
      filter(
        (endpoint %>%
          str_detect('copd|dm|ihd|lc|stroke') &
          as.integer(agegroup) >= 25) |
          endpoint == 'lri'
      ) %>%
      ungroup
  } else if (CR == "O3") {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = 'copd',
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      fill(RR) %>%
      filter(agegroup != 'ALL')
  } else if (CR == "NO2") {
    expand_grid(
      concentration = RR_table[[RR_index]] %>% pull(concentration),
      endpoint = 'cause', #ALL CAUSE
      agegroup = c('ALL', seq(15, 95, 5) %>% matchable(0))
    ) %>%
      left_join(RR_tbl) %>%
      fill(RR) %>%
      filter(agegroup != 'ALL')
  }

  # ---- Guard: RR_reshape has data after reshape ----
  if (nrow(RR_reshape) == 0) {
    stop(
      "RR_reshape is empty for model \"",
      CR,
      "\" with index \"",
      RR_index,
      "\". This should not happen for built-in models. Check the RR lookup table."
    )
  }

  # ---- Guard: no remaining NA in RR after fill ----
  na_rr <- RR_reshape %>% filter(is.na(RR))
  if (nrow(na_rr) > 0) {
    warning(
      nrow(na_rr),
      " NA values remain in RR_reshape after fill. ",
      "Common causes: (1) RR lookup table is missing endpoint/agegroup combinations ",
      "at certain concentrations; (2) the 'ALL' fill row is missing for some endpoints. ",
      "These rows will be dropped downstream. ",
      "First few: concentration=",
      paste(head(unique(na_rr$concentration), 3), collapse = ", ")
    )
  }

  return(RR_reshape)
}

#' format mortality rate data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame, contains domain, endpoint name
#'
#' @examples
getMortRate <- function(at) {
  # ---- Guard: MortRate loaded ----
  if (!exists('MortRate', envir = globalenv())) {
    stop("MortRate not found in global environment. Run read_files() first.")
  }
  # ---- Guard: year column exists ----
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
  MortRate %>%
    mutate(endpoint = tolower(endpoint)) %>%
    select(domain, endpoint, agegroup, MortRate = {
      at
    })
}

#' get Conc_real data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame, contains x-y and the concentration
#'
#' @examples
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
  Conc_real %>%
    select(x, y, concentration = {
      at
    })
}

#' get Conc_cf data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame, contains x-y and the concentration
#'
#' @examples
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
  Conc_cf %>%
    select(x, y, concentration = {
      at
    })
}

#' get Pop data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame, contains x-y and the population
#'
#' @examples
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
  Pop %>%
    select(x, y, Pop = {
      at
    })
}

#' get AgeGroup data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame, contains x-y and the population
#'
#' @examples
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
  AgeGroup %>%
    select(domain, agegroup, AgeStruc = {
      at
    })
}

#' Calculate gridded PM2.5 attributed mortality
#'
#' **PAF methodology — PWRR normalization:**
#'
#' Baseline mortality rates (`mRate`) are domain-level survey data (e.g., national or
#' provincial statistics), not grid-specific rates. Under the relative-risk principle:
#'
#'   I_g / I_domain = RR(C_g) / PWRR_domain
#'
#' where `I_g` is the mortality rate at grid concentration C_g, and `I_domain` is the
#' observed domain-average rate. This gives a uniform zero-PM2.5 baseline within each domain:
#'
#'   I_0 = I_domain / PWRR_domain    (same for all grids in the domain)
#'
#' Attributable mortality for grid g vs. zero-PM2.5 (TMREL):
#'
#'   ΔMort_g = Pop_g × AgeStruc × (I_g − I_0)
#'           = Pop_g × AgeStruc × MortRate_domain × (RR(C_g) − 1) / PWRR_domain
#'
#' Compared to the standard grid-level PAF `(RR_g−1)/RR_g`, which implies I_0 = MortRate/RR_g
#' (varying per grid), the PWRR approach gives a physically consistent I_0 within each domain.
#'
#' @param Grids a vector of grid coords
#' @param Conc_r a 3-column `data.frame` stores real PM2.5 concentration of each grid, used for PWRR calculation
#' @param Conc_c a 3-column `data.frame` stores counterfactual PM2.5 concentration of each grid, used for the RR numerator. Defaults to Conc_r
#' @param ag proportions of 20 age-groups inside the population structure
#' @param mRate the domain-level baseline mortality rates per endpoint and age group
#' @param pop a 3-column dataframe stores population volume of each grid
#' @param CI RR branch: "MEAN" (default), "UP", "LOW", or "RANGE"
#' @param domain the spatial aggregation level (e.g., "Country", "Province") for PWRR computation
#'
#' @return When CI is "MEAN"/"UP"/"LOW", a wide table of death estimates per endpoint-agegroup per grid.
#'   When CI is "RANGE", the table also includes _UP and _LOW suffixed columns for the 95% CI bounds.
#'
#' @examples
