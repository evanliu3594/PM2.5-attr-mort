# Data loading & RR lookup module
#   - matchable(), normalize_coords()  — coordinate & numeric normalisation
#   - read_files()                     — load all input data to global env
#   - getMortRate(), getConc_real(), getConc_cf(), getPop(), getAgeGroup() — accessors
#   Modified 260618: type-agnostic coordinate matching (any_of + as.numeric),
#     Conc_cf defaults to NULL, domain column auto-normalisation (Country→domain).

# Standardised RR lookup table (built by set_Model() via RR_std(), read by Mortality/Uncertainty)
.RR_std_tbl <- NULL

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
  num |> round(dgt) |> str_c()
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
    df <- df |> rename(x = !!lon_col, y = !!lat_col)
    log_msg(
      INFO,
      "Coordinates normalized: \"{lon_col}\" -> \"x\", \"{lat_col}\" -> \"y\""
    )
  }

  return(df)
}

#' Load all input data into the global environment
#'
#' Reads gridded (Grid_info, Pop, Conc_real, Conc_cf) and domain-level
#' (MortRate, AgeGroup) Excel/CSV files, normalises coordinate and domain
#' column names, converts join keys to matchable strings, and assigns the
#' resulting data frames to \code{globalenv()}.
#'
#' @param Grids path to grid information (.xlsx or .csv)
#' @param Pop path to population (.xlsx or .csv)
#' @param Conc_real path to real concentration (.xlsx or .csv)
#' @param Conc_cf path to counter-fact concentration (.xlsx or .csv).
#'   Default \code{NULL} — counterfactual scenarios fall back to Conc_real.
#' @param MortRate path to mortality rate (.xlsx or .csv)
#' @param AgeGroup path to population age structure (.xlsx or .csv)
#' @param dgt_grid int, digit of map grid coordinates (default 2)
#' @param dgt_conc int, digit of concentrations (default 1)
#'
#' @return assign data to global env.
#' @export
read_files <- function(
  Grids,
  Pop,
  Conc_real,
  Conc_cf = NULL,
  MortRate,
  AgeGroup,
  dgt_grid = 2,
  dgt_conc = 1
) {
  #  Guard: file existence
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

  assign('Conc_real', envir = globalenv(), conc_real_df)

  # Specify UNREAL PM2.5 data, used for only counter-fact scenario.

  if (!is.null(Conc_cf) && file.exists(Conc_cf)) {
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
  } else {
    conc_cf_df <- NULL
  }

  assign('Conc_cf', envir = globalenv(), conc_cf_df)

  if (is.null(conc_cf_df)) {
    log_msg(
      WARN,
      "Conc_cf file \"{Conc_cf}\" not found — set to NULL. ",
      "Counterfactual scenarios will fall back to real concentrations."
    )
  }

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

  assign(
    'MortRate',
    envir = globalenv(),
    mort_rate_raw |>
      mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  )

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

  assign(
    "AgeGroup",
    envir = globalenv(),
    age_group_raw |>
      mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  )

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

#' format mortality rate data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame with columns: domain, endpoint, agegroup, MortRate
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
  # fmt: skip
  Conc_real |> select(x, y, concentration = {at})
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

  # fmt: skip
  Conc_cf |> select(x, y, concentration = {at})
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
  # fmt: skip
  Pop |> select(x, y, Pop = {at})
}

#' get AgeGroup data
#'
#' @param at the year/scenario name to choose
#'
#' @return data.frame with columns: domain, agegroup, and age structure proportion
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

  # fmt: skip
  AgeGroup |> select(domain, agegroup, AgeStruc = {at})
}
