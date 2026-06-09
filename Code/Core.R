# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09
# Modified By yifan liu on 23/12/19
# Improved By Yifan LIU With Deepseek on 260610

library(tidyverse)
library(writexl)
library(readxl)

#' detect CRF model name and generate a name for file output
#'
#' @return formatted string of CRF name
#' @export
#'
tell_Model <- function() {
  return(
    if (.CR_Model %>% str_detect('IER')) {
      str_c(.CR_Model)
    } else if (.CR_Model %in% c('5COD', 'NCD+LRI')) {
      str_c('PM2.5_GEMM', .CR_Model, sep = '_')
    } else if (.CR_Model == 'MRBRT') {
      str_c("PM2.5_", .CR_Model)
    } else if (.CR_Model == 'O3') {
      #新增
      str_c(.CR_Model)
    } else if (.CR_Model == 'NO2') {
      str_c(.CR_Model)
    } #新增
  )
}

#' set CRF for calculation
#'
#' @param Model string, one of `IER`, `NCD+LRI`, `5COD`, `MRBRT`, `O3` or `NO2`
#'
#' @export
set_Model <- function(Model) {
  assign(".CR_Model", Model, envir = globalenv())

  if (str_detect(Model, "IER|NCD\\+LRI|5COD|MRBRT|O3|NO2")) {
    cat(str_glue(
      "C-R Model \"{tell_Model()}\" is set as the default methodology"
    ))
  } else {
    warning(str_glue(
      "\"{Model}\" is not a bnuilt-in CR model, \\
      please provide a corrosponding `RR_table` after `read_file()`"
    ))
  }
}

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
#' @param RR param passed to `RR_std()`
#' @param domain the spatial aggregation level (e.g., "Country", "Province") for PWRR computation
#'
#' @return a table of death estimates for each endpoint & age-groups(columns) for every grids(rows)
#'
#' @examples
Mortality <- function(
  Grids,
  Conc_r,
  Conc_c = NULL,
  ag,
  mRate,
  pop,
  RR,
  domain = NULL
) {
  # ---- Guard: input presence ----
  if (is.null(Conc_c)) {
    Conc_c <- Conc_r
  }

  if (is.null(domain)) {
    stop(
      "No grouping domain specified — cannot compute PWRR. ",
      "Pass domain = \"Country\" or the column name in Grid_info that defines your aggregation units."
    )
  }

  # ---- Guard: domain column exists ----
  if (!all(domain %in% names(Grids))) {
    stop(
      "Domain column \"",
      paste(setdiff(domain, names(Grids)), collapse = "\", \""),
      "\" not found in Grid_info. ",
      "Available columns: ",
      paste(names(Grids), collapse = ", ")
    )
  }

  RR_tbl <- RR_std(RR)

  # ---- Guard: RR lookup table not empty ----
  if (nrow(RR_tbl) == 0) {
    stop(
      "RR lookup table is empty for index \"",
      RR,
      "\". Check that RR_table[[\"",
      RR,
      "\"]] exists and contains data."
    )
  }

  # ---- Guard: concentration range vs RR lookup ----
  conc_range_r <- range(as.numeric(Conc_r$concentration), na.rm = TRUE)
  conc_range_c <- range(as.numeric(Conc_c$concentration), na.rm = TRUE)
  rr_conc_range <- range(as.numeric(RR_tbl$concentration), na.rm = TRUE)

  if (
    conc_range_r[1] < rr_conc_range[1] || conc_range_r[2] > rr_conc_range[2]
  ) {
    warning(
      "Conc_r contains concentrations [",
      conc_range_r[1],
      ", ",
      conc_range_r[2],
      "] outside RR lookup range [",
      rr_conc_range[1],
      ", ",
      rr_conc_range[2],
      "]. These grids will get NA RR and be dropped by na.omit."
    )
  }

  if (
    conc_range_c[1] < rr_conc_range[1] || conc_range_c[2] > rr_conc_range[2]
  ) {
    warning(
      "Conc_c contains concentrations [",
      conc_range_c[1],
      ", ",
      conc_range_c[2],
      "] outside RR lookup range [",
      rr_conc_range[1],
      ", ",
      rr_conc_range[2],
      "]. These grids will get NA RR and be dropped by na.omit."
    )
  }

  # ---- Guard: coordinate overlap ----
  grid_xy <- Grids %>% select(x, y) %>% distinct()
  conc_xy <- Conc_r %>% select(x, y) %>% distinct()
  pop_xy <- pop %>% select(x, y) %>% distinct()
  orphan_grids <- grid_xy %>% anti_join(conc_xy, by = c("x", "y"))

  if (nrow(orphan_grids) > 0) {
    warning(
      nrow(orphan_grids),
      " grid(s) in Grid_info have no matching concentration in Conc_r. ",
      "They will be dropped by na.omit."
    )
  }

  orphan_pop <- grid_xy %>% anti_join(pop_xy, by = c("x", "y"))
  if (nrow(orphan_pop) > 0) {
    warning(
      nrow(orphan_pop),
      " grid(s) in Grid_info have no matching population in pop. ",
      "They will be dropped by na.omit."
    )
  }

  # ---- PWRR calculation ----
  n_grid_total <- nrow(Grids %>% select(x, y) %>% distinct())

  PWRR_pre <- list(Grids, Conc_r, pop, RR_tbl) %>%
    reduce(left_join)

  n_before <- nrow(PWRR_pre)
  PWRR_data <- PWRR_pre %>% na.omit
  n_after <- nrow(PWRR_data)

  if (n_after == 0) {
    stop(
      "PWRR step: all ",
      n_before,
      " rows dropped by na.omit. ",
      "Check that concentration values fall within the RR lookup table range, ",
      "and that Grid_info, Conc_r, and pop share common (x, y) coordinates."
    )
  }

  if (n_after < n_before * 0.5) {
    warning(
      "PWRR step: ",
      n_before - n_after,
      " of ",
      n_before,
      " rows (",
      round(100 * (1 - n_after / n_before)),
      "%) dropped by na.omit. ",
      "Check concentration range vs RR lookup and coordinate consistency."
    )
  }

  PWRR <- PWRR_data %>%
    group_by(pick(all_of(domain))) %>%
    summarise(PWRR = weighted.mean(RR, Pop, na.rm = TRUE), .groups = "drop")

  # ---- Guard: PWRR validity ----
  # PWRR >= 1 is valid (PWRR = 1 means all grids at or below TMREL).
  # Only PWRR < 1 is physically impossible for PM2.5.
  if (
    any(is.na(PWRR$PWRR)) || any(is.infinite(PWRR$PWRR)) || any(PWRR$PWRR < 1)
  ) {
    stop(
      "Invalid PWRR detected. ",
      if (any(is.na(PWRR$PWRR))) {
        "Some domains have NA PWRR (no valid grids after na.omit). "
      },
      if (any(is.infinite(PWRR$PWRR))) "Some domains have Inf PWRR. ",
      if (any(PWRR$PWRR < 1)) {
        "Some domains have PWRR < 1 (RR cannot be < 1 for PM2.5). "
      },
      "Check concentration and population data for these domains: ",
      paste(
        PWRR[[domain]][
          is.na(PWRR$PWRR) | is.infinite(PWRR$PWRR) | PWRR$PWRR < 1
        ],
        collapse = ", "
      )
    )
  }

  # ---- Main mortality calculation ----
  mort_pre <- list(
    Grids,
    Conc_c,
    pop,
    RR_tbl,
    mRate %>% rename({{ domain }} := domain),
    ag %>% rename({{ domain }} := domain),
    PWRR
  )

  n_before_mort <- nrow(reduce(mort_pre, left_join))
  mort_data <- mort_pre %>% reduce(left_join) %>% na.omit
  n_after_mort <- nrow(mort_data)

  if (n_after_mort == 0) {
    stop(
      "Mortality step: all ",
      n_before_mort,
      " rows dropped by na.omit. ",
      "Common causes: (1) age groups in mRate/AgeGroup don't match RR_tbl's agegroup list; ",
      "(2) endpoint names don't match between mRate and RR_tbl; ",
      "(3) domain names differ between Grid_info, mRate, and AgeGroup. ",
      "Check with Mortality_debug() or inspect the join intermediates."
    )
  }

  if (n_after_mort < n_before_mort * 0.5) {
    warning(
      "Mortality step: ",
      n_before_mort - n_after_mort,
      " of ",
      n_before_mort,
      " rows (",
      round(100 * (1 - n_after_mort / n_before_mort)),
      "%) dropped by na.omit."
    )
  }

  result <- mort_data %>%
    mutate(
      Mort = Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5
    ) %>%
    select(x, y, endpoint, agegroup, Mort) %>%
    pivot_wider(
      names_from = c('endpoint', 'agegroup'),
      names_sep = '_',
      values_from = 'Mort'
    )

  # ---- Guard: result not empty ----
  if (nrow(result) == 0) {
    stop(
      "Mortality result has 0 rows. ",
      "This should not happen after passing the na.omit check — ",
      "check pivot_wider inputs."
    )
  }

  return(result)
}


#' Calculate Attributable Mortality at a certain year/scenario
#'
#' @param at year/scenario
#' @param RR RR branch
#' @param domain grid domain
#'
#' @return
#' @export
#'
#' @examples
Mortality_at <- function(at, RR = "MEAN", domain) {
  Mortality(
    Grids = Grid_info,
    Conc_r = getConc_real(at),
    Conc_c = NULL,
    pop = getPop(at),
    ag = getAgeGroup(at),
    mRate = getMortRate(at),
    RR = RR,
    domain = domain
  )
}


#' Uncertainties Calculation
#'
#' @param m_Rate 死亡率数据
#' @param PWE 分地区人口加权浓度
#' @param aggr_pop 分区汇总人口
#' @param age_struc 分区年龄结构
#' @param includeConc 浓度不确定性开关
#' @param Conc_ERR 浓度数据相对不确定性（百分比），默认 12 表示 +/-12%
#'
#' @export
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#'
#' @examples
Uncertainty <- function(
  PWE,
  aggr_pop,
  age_struc,
  m_Rate,
  includeConc = FALSE,
  Conc_ERR = 12
) {
  if (includeConc) {
    warning(str_glue(
      "Including concentration uncertainty at +/-{Conc_ERR}% of PWE."
    ))
  }

  PWE <- PWE %>%
    na.omit %>%
    rename_with(~'domain', where(is.character)) %>%
    rename_with(~'concentration', where(is.numeric))

  aggr_pop <- aggr_pop %>%
    na.omit %>%
    rename_with(~'domain', where(is.character))

  RR_base <- PWE %>%
    mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std('MEAN')) %>%
    mutate(PAF_base = 1 - 1 / RR) %>%
    select(-concentration, -RR)

  RR_test_up <- PWE %>%
    mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std('UP')) %>%
    mutate(PAF_test = 1 - 1 / RR) %>%
    select(-concentration, -RR)

  RR_test_low <- PWE %>%
    mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std('LOW')) %>%
    mutate(PAF_test = 1 - 1 / RR) %>%
    select(-concentration, -RR)

  test_PAF_up <- left_join(RR_base, RR_test_up) %>%
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) %>%
    pivot_wider(names_from = 'varname', values_from = 'PAF_test') %>%
    mutate(across(
      matches("^test_CR"),
      ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
    ))

  test_PAF_low <- left_join(RR_base, RR_test_low) %>%
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) %>%
    pivot_wider(names_from = 'varname', values_from = 'PAF_test') %>%
    mutate(across(
      matches("^test_CR"),
      ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
    ))

  if (includeConc) {
    # Perturb concentration by +/- Conc_ERR%, look up new RR, compute perturbed PAF.
    # Must preserve domain, endpoint, agegroup for 1:1 join with RR_base below.
    conc_perturb_up <- PWE %>%
      mutate(
        concentration = matchable(concentration * (1 + Conc_ERR / 100), 1)
      ) %>%
      left_join(RR_std('MEAN'), by = "concentration") %>%
      mutate(PAF_test = 1 - 1 / RR) %>%
      select(domain, endpoint, agegroup, PAF_test)

    test_PAF_up <- test_PAF_up %>%
      left_join(
        conc_perturb_up %>%
          left_join(RR_base, by = c("domain", "endpoint", "agegroup")) %>%
          mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) %>%
          pivot_wider(names_from = 'varname', values_from = 'PAF_test') %>%
          mutate(across(
            matches("^test_Pollu"),
            ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
          )) %>%
          select(-PAF_base),
        by = c("domain", "endpoint", "agegroup")
      )

    conc_perturb_low <- PWE %>%
      mutate(
        concentration = matchable(concentration * (1 - Conc_ERR / 100), 1)
      ) %>%
      left_join(RR_std('MEAN'), by = "concentration") %>%
      mutate(PAF_test = 1 - 1 / RR) %>%
      select(domain, endpoint, agegroup, PAF_test)

    test_PAF_low <- test_PAF_low %>%
      left_join(
        conc_perturb_low %>%
          left_join(RR_base, by = c("domain", "endpoint", "agegroup")) %>%
          mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) %>%
          pivot_wider(names_from = 'varname', values_from = 'PAF_test') %>%
          mutate(across(
            matches("^test_Pollu"),
            ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
          )) %>%
          select(-PAF_base),
        by = c("domain", "endpoint", "agegroup")
      )
  }

  Sensi_up <- list(test_PAF_up, aggr_pop, age_struc, m_Rate) %>%
    reduce(left_join) %>%
    mutate(across(
      matches('^test'),
      ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5
    )) %>%
    select(matches('^test')) %>%
    map_df(sum, na.rm = T) %>%
    pivot_longer(
      matches('^test'),
      names_to = c("item", 'domain', 'endpoint', 'agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )

  Sensi_low <- list(test_PAF_low, aggr_pop, age_struc, m_Rate) %>%
    reduce(left_join) %>%
    mutate(across(
      matches('^test'),
      ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5
    )) %>%
    select(matches('^test')) %>%
    map_df(sum, na.rm = T) %>%
    pivot_longer(
      matches('^test'),
      names_to = c("item", 'domain', 'endpoint', 'agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )

  test_sigma_up <- bind_rows(
    left_join(RR_base, RR_test_up) %>%
      mutate(sigma = abs(PAF_test - PAF_base), .keep = 'unused') %>%
      add_column(item = 'CR'),
    PWE %>%
      bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') %>%
      mutate(sigma = Conc_ERR, .keep = 'unused')
  )

  test_sigma_low <- bind_rows(
    left_join(RR_base, RR_test_low) %>%
      mutate(sigma = abs(PAF_test - PAF_base), .keep = 'unused') %>%
      add_column(item = 'CR'),
    PWE %>%
      bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') %>%
      mutate(sigma = Conc_ERR, .keep = 'unused')
  )

  # Error propagation: σ_M² = Σ (∂M/∂x_i)² × σ_xi²
  # With finite-difference: ∂M/∂x_i ≈ ΔM_i / σ_xi, where ΔM_i (= Sensi) is the
  # mortality change from a 1σ perturbation. Therefore σ_M² = Σ Sensi_i².
  # (test_sigma_* tables are retained for diagnostic inspection if needed.)
  left_join(
    Sensi_up %>%
      group_by(domain) %>%
      summarise(CI_UP = sqrt(sum(Sensi^2)), .groups = "drop"),
    Sensi_low %>%
      group_by(domain) %>%
      summarise(CI_LOW = sqrt(sum(Sensi^2)), .groups = "drop"),
    by = "domain"
  ) %>%
    return()
}

Mort_Aggregate <- function(
  full_result,
  domain = 'Grid',
  by = NULL,
  write = T,
  ...
) {
  # ---- Guard: full_result not empty ----
  if (is.null(full_result) || length(full_result) == 0) {
    stop(
      "full_result is NULL or empty. Mortality_at() must return results before aggregation."
    )
  }

  if (!is.list(full_result)) {
    stop(
      "full_result must be a named list of data frames (output of map(~ Mortality_at(...))). ",
      "Got: ",
      class(full_result)[1]
    )
  }

  # ---- Guard: domain validity ----
  if (domain != 'Grid') {
    if (!all(domain %in% names(Grid_info))) {
      stop(
        "Domain column \"",
        paste(setdiff(domain, names(Grid_info)), collapse = "\", \""),
        "\" not found in Grid_info. Available: ",
        paste(names(Grid_info), collapse = ", ")
      )
    }
  }

  # ---- Guard: by column validity (for Grid domain where column names are endpoint_agegroup) ----
  valid_by <- c("endpoint", "agegroup", names(Grid_info))
  if (!is.null(by) && !by %in% valid_by) {
    stop(
      "by = \"",
      by,
      "\" is not a valid aggregation dimension. ",
      "Use \"endpoint\", \"agegroup\", or a column name from Grid_info: ",
      paste(setdiff(names(Grid_info), c("x", "y")), collapse = ", ")
    )
  }

  pre_aggr_result <- if (domain == 'Grid' & is.null(by)) {
    full_result
  } else if (domain == 'Grid' & !is.null(by)) {
    full_result %>%
      map(
        ~ .x %>%
          pivot_longer(
            cols = matches('_[1-9]?(0|5)$'),
            values_to = 'Mort',
            names_to = c('endpoint', 'agegroup'),
            names_sep = '_'
          ) %>%
          group_by(pick(all_of(c(domain, by)))) %>%
          summarise(Mort = sum(Mort)) %>%
          ungroup %>%
          pivot_wider(names_from = by, values_from = 'Mort')
      )
  } else if (domain != 'Grid' & is.null(by)) {
    full_result %>%
      map(
        ~ left_join(.x, Grid_info) %>%
          group_by(pick(all_of(domain))) %>%
          summarise(across(matches('_[1-9]?(0|5)$'), sum)) %>%
          ungroup
      )
  } else if (domain != 'Grid' & !is.null(by)) {
    full_result %>%
      map(
        ~ .x %>%
          left_join(Grid_info) %>%
          pivot_longer(
            cols = matches('_[1-9]?(0|5)$'),
            names_to = c('endpoint', 'agegroup'),
            names_sep = '_',
            values_to = 'Mort'
          ) %>%
          group_by(pick(all_of(c(domain, by)))) %>%
          summarise(Mort = sum(Mort)) %>%
          ungroup %>%
          pivot_wider(names_from = by, values_from = 'Mort')
      )
  }

  # ---- Guard: aggregation produced non-empty results ----
  empty_results <- pre_aggr_result %>% map_lgl(~ nrow(.x) == 0)
  if (any(empty_results)) {
    warning(
      "Some scenarios produced empty aggregation results: ",
      paste(names(full_result)[empty_results], collapse = ", "),
      ". ",
      "Check that the domain column has valid values for all grids."
    )
  }

  pre_aggr_result <- pre_aggr_result %>%
    map(
      if (domain == 'Grid') {
        ~ .x %>%
          mutate(
            Total = rowSums(select(., matches('_[1-9]?(0|5)$')), na.rm = TRUE),
            .after = x:y
          )
      } else {
        ~ .x %>%
          mutate(
            Total = rowSums(select(., -all_of(!!domain)), na.rm = TRUE),
            .after = !!domain
          )
      }
    )

  CI <- if (domain == 'Grid') {
    full_result %>%
      map(
        ~ Grid_info %>% select(x:y, any_of(c("Country", "Region", "Province")))
      )
  } else {
    names(full_result) %>%
      set_names %>%
      map(
        ~ Uncertainty(
          includeConc = list(...)[["includeConc"]] %||% FALSE,
          Conc_ERR = list(...)[["Conc_ERR"]] %||% 12,
          m_Rate = getMortRate(.x),
          aggr_pop = Grid_info %>%
            left_join(getPop(.x)) %>%
            group_by(pick(all_of(domain))) %>%
            summarise(Pop = sum(Pop, na.rm = T)) %>%
            na.omit,
          age_struc = getAgeGroup(.x),
          PWE = list(Grid_info, getConc_real(.x), getPop(.x)) %>%
            reduce(left_join) %>%
            na.omit %>%
            group_by(pick(all_of(domain))) %>%
            summarise(
              PWE = weighted.mean(as.numeric(concentration), Pop, na.rm = T)
            )
        ) %>%
          rename_with(~domain, where(is.character))
      )
  }

  # ---- Guard: CI computed successfully ----
  if (domain != 'Grid') {
    empty_ci <- CI %>% map_lgl(~ is.null(.x) || nrow(.x) == 0)
    if (any(empty_ci)) {
      warning(
        "Uncertainty CI could not be computed for: ",
        paste(names(full_result)[empty_ci], collapse = ", "),
        ". ",
        "Check that aggr_pop and PWE have matching domain values with age_struc and m_Rate."
      )
    }
  }

  aggr_result <- map2(CI, pre_aggr_result, ~ left_join(.x, .y))

  if (domain %in% c("Country", "Province", "Region")) {
    aggr_result <- aggr_result %>%
      imap_dfr(~ .x %>% add_column(year = .y, .before = TRUE))
  }

  # ---- Guard: final result not empty ----
  if (is.data.frame(aggr_result) && nrow(aggr_result) == 0) {
    stop(
      "Aggregation produced 0 rows. Check domain/region mapping consistency."
    )
  }

  if (write) {
    by_label <- if (is.null(by)) {
      "Everything"
    } else {
      str_replace(by, "^.{1}", toupper)
    }
    outpath <- str_glue(
      "./Result/{tell_Model()}_{domain}_by{by_label}_\\
       {head(names(full_result),1)}-\\
       {tail(names(full_result),1)}_\\
       Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    )
    # Ensure output directory exists
    dir.create("./Result", showWarnings = FALSE, recursive = TRUE)
    aggr_result %>% write_xlsx(outpath)
    cat("Result written to: ", outpath, "\n")
  }

  return(aggr_result)
}
