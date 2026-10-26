#======================================================================================#
#  Automated data pre-processing — generate all instance files required by the
#  mortality calculation pipeline from raw source data.
#
#  Usage: edit paths in the CONFIG section below, then source this file.
#  Output: Grid_info / GridPM25 / GridPop / GBD_mortality /
#          GBD_agestructure instance .xlsx files under ./Data/.
#
#  Dependencies: tidyverse, terra, sf, rnaturalearth, readxl, writexl, llmjoin
#  LLM setup: set_llm(provider = "...", key = "...", model = "...")
#  See: https://github.com/evanliu3594/llmjoin
#======================================================================================#

# install.packages(c("terra","sf","llmjoin"))

library(tidyverse)
library(writexl)
library(readxl)
library(jsonlite)
library(llmjoin)
library(terra)
library(sf)
sf_use_s2(FALSE)

# ══════════════════════════════════════════════════════════════════════════════════════
#  CONFIG — edit paths and parameters below ----
# ══════════════════════════════════════════════════════════════════════════════════════

# Grid resolution (degrees)
GRID_RES <- 0.5

# Source data paths, replace with yours
PM25_RAW_DIR <- Sys.getenv("PM25_RAW_DIR", unset = "./Data/PM25_raw/")
POP_RAW_DIR <- Sys.getenv("PM25_POP_RAW_DIR", unset = "")
MORT_RAW_DIR <- Sys.getenv("PM25_MORT_RAW_DIR", unset = "./Data/IHME_MortRate_raw/")
AGESTRUC_RAW_DIR <- Sys.getenv("PM25_AGESTRUC_RAW_DIR", unset = "./Data/IHME_Pop_raw/")

# Output
OUT_DIR <- Sys.getenv("PM25_INSTANCE_DIR", unset = "./Data/")
OUT_SUFFIX <- Sys.getenv(
  "PM25_INSTANCE_SUFFIX",
  unset = format(Sys.Date(), "%y%m%d")
)
if (!str_detect(OUT_SUFFIX, "^[0-9]{6}$")) {
  stop("PM25_INSTANCE_SUFFIX must contain exactly six digits.")
}

# Manually set future year / counter-fact scenario data mapping
# scenario → Year mapping (source year → output column name)
YEAR_MAP <- c(
  "2015" = "base2015",
  "2020" = "SSP1-Baseline_2030"
)

# Every scenario must declare the source year of each component. The 2030
# label currently combines 2030 exposure with 2020 population/mortality and
# 2019 age structure. This is a proxy, not a complete 2030 projection.
ALLOW_MIXED_SCENARIO_YEARS <- FALSE
SCENARIO_YEAR_SPEC <- tribble(
  ~scenario, ~exposure_year, ~population_year, ~age_year, ~mortality_year,
  "base2015", 2015, 2015, 2015, 2015,
  "SSP1-Baseline_2030", 2030, 2020, 2019, 2020
)

validate_scenario_years <- function(spec, allow_mixed) {
  component_cols <- c(
    "exposure_year", "population_year", "age_year", "mortality_year"
  )
  mixed <- spec |>
    mutate(
      n_years = apply(
        pick(all_of(component_cols)),
        1,
        \(x) length(unique(x))
      )
    )
  bad <- mixed |> filter(n_years > 1)
  if (nrow(bad) > 0 && !allow_mixed) {
    details <- bad |>
      mutate(
        detail = str_c(
          scenario, " (",
          "exposure=", exposure_year,
          ", population=", population_year,
          ", age=", age_year,
          ", mortality=", mortality_year,
          ")"
        )
      ) |>
      pull(detail)
    stop(
      "Mixed source years are blocked for scenario(s): ",
      paste(details, collapse = "; "),
      ". Set ALLOW_MIXED_SCENARIO_YEARS <- TRUE only for an approved proxy. ",
      "The build manifest records the chosen years."
    )
  }
  invisible(mixed)
}

validate_scenario_years(SCENARIO_YEAR_SPEC, ALLOW_MIXED_SCENARIO_YEARS)

if (!nzchar(POP_RAW_DIR) || !dir.exists(POP_RAW_DIR)) {
  stop(
    "Population raster directory not found. Set PM25_POP_RAW_DIR or edit ",
    "POP_RAW_DIR in build_instance.R."
  )
}
pop_files <- list.files(
  POP_RAW_DIR,
  pattern = "\\d+\\.(tif|tiff)$",
  full.names = TRUE,
  recursive = TRUE
)
if (length(pop_files) == 0) {
  stop("No .tif files found in ", POP_RAW_DIR)
}

# Helper function to rename year/scenario column to year
map_year <- \(yr) recode(yr, !!!YEAR_MAP) |> coalesce(yr)

# Age parsing: "<5" → "0", otherwise extract leading 1–2 digits, empty → "0"
parse_age <- \(x) str_extract(x, "^\\d{1,2}") |> replace_na("0")

# IHME cause → abbreviation
map_cause <- c(
  "Tracheal, bronchus, and lung cancer" = "LC",
  "Lower respiratory infections" = "LRI",
  "Stroke" = "STROKE",
  "Ischemic heart disease" = "IHD",
  "Non-communicable diseases" = "NCD",
  "NCD+LRI" = "NCD+LRI",
  "Chronic obstructive pulmonary disease" = "COPD",
  "Diabetes mellitus type 2" = "DM2",
  "All causes" = "ALLCAUSE"
)

# Country mapping is expensive and depends on an external LLM. Cache the
# result so later rebuilds do not depend on API availability.
read_or_build_country_map <- function(
  grid_df,
  target_df,
  key1,
  key2,
  cache_path
) {
  if (file.exists(cache_path)) {
    mapping <- read_csv(cache_path, show_col_types = FALSE)
  } else {
    mapping <- llmjoin::build_joint(
      grid_df,
      target_df,
      key1 = key1,
      key2 = key2
    )
    write_csv(mapping, cache_path)
  }
  required <- c("Country", "domain")
  if (!all(required %in% names(mapping))) {
    stop(
      "Country mapping must contain Country and domain. Found: ",
      paste(names(mapping), collapse = ", ")
    )
  }
  if (anyDuplicated(mapping$Country) > 0 || anyDuplicated(mapping$domain) > 0) {
    stop("Country mapping contains duplicated Country or domain keys.")
  }
  mapping
}


# ══════════════════════════════════════════════════════════════════════════════════════
#  1 Grid information (terra::rasterize country polygons) ----
# ══════════════════════════════════════════════════════════════════════════════════════

cat("\n── 1. Grid information ──\n")

# Auto-detect template CRS and extent from the first PM2.5 NetCDF file
nc_files <- list.files(PM25_RAW_DIR, pattern = "\\.nc$", full.names = TRUE)

if (length(nc_files) == 0) {
  stop("No .nc files found in ", PM25_RAW_DIR)
}
if (length(list.files(MORT_RAW_DIR, pattern = "\\.(csv|zip)$")) == 0) {
  stop("No mortality CSV/ZIP files found in ", MORT_RAW_DIR)
}
if (length(list.files(AGESTRUC_RAW_DIR, pattern = "(csv|zip)$")) == 0) {
  stop("No age-structure CSV/ZIP files found in ", AGESTRUC_RAW_DIR)
}

ref_rast <- rast(nc_files[[1]])
GRID_CRS <- crs(ref_rast)
GRID_EXT <- ext(ref_rast)

template <- rast(GRID_EXT, resolution = GRID_RES, crs = GRID_CRS)
template_fine <- rast(GRID_EXT, resolution = GRID_RES / 10, crs = GRID_CRS)

cat(str_glue(
  "  CRS: {GRID_CRS}\n  extent: {paste(as.vector(GRID_EXT), collapse=', ')}\n"
))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  select(name_long) |>
  st_transform(GRID_CRS)

grid_info <- rasterize(
  vect(world),
  template_fine,
  field = "name_long",
  touches = TRUE
) |>
  resample(template, method = "modal") |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  rename(Country = name_long) |>
  mutate(across(x:y, ~ round(.x, 3)))

missing_countries <- setdiff(world$name_long, grid_info$Country)
cat(str_glue(
  "  ({length(missing_countries)} countries dropped due to grid resolution: ",
  "{str_c(sort(missing_countries), collapse = ', ')}}\n"
))

write_xlsx(
  grid_info,
  file.path(OUT_DIR, str_glue("Grid_info_instance_{OUT_SUFFIX}.xlsx"))
)
cat(str_glue(
  "  → {nrow(grid_info)} grids, {length(unique(grid_info$Country))} countries\n"
))

# ══════════════════════════════════════════════════════════════════════════════════════
#  2 PM2.5 concentration ----
# ══════════════════════════════════════════════════════════════════════════════════════

cat("\n── 2. PM2.5 concentration ──\n")

PM25_FILE_PREFIX <- "Annual_pm25_"

pm25 <- map(
  nc_files,
  ~ {
    nm <- basename(.x) |> str_remove("\\.nc$")
    if (nzchar(PM25_FILE_PREFIX)) {
      nm <- str_remove(nm, fixed(PM25_FILE_PREFIX))
    }
    r <- rast(.x)
    if (nlyr(r) > 1) {
      r <- app(r, mean, na.rm = TRUE)
    }
    # Resample to template grid so coordinates match Grid_info and GridPop
    r <- if (same.crs(r, template)) {
      resample(r, template, method = "bilinear")
    } else {
      project(r, template, method = "bilinear")
    }
    return(setNames(r, nm))
  }
) |>
  reduce(c) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  mutate(across(x:y, ~ round(.x, 3) |> as.character()))

pm25_scenarios <- setdiff(names(pm25), c("x", "y"))
if (!setequal(pm25_scenarios, SCENARIO_YEAR_SPEC$scenario)) {
  stop(
    "PM2.5 files produce scenarios [", paste(pm25_scenarios, collapse = ", "),
    "], but SCENARIO_YEAR_SPEC declares [",
    paste(SCENARIO_YEAR_SPEC$scenario, collapse = ", "), "]."
  )
}

write_xlsx(
  pm25,
  file.path(OUT_DIR, str_glue("GridPM25_instance_{OUT_SUFFIX}.xlsx"))
)
cat(str_glue("  → {nrow(pm25)} grids, {ncol(pm25) - 2} scenarios\n"))

# ══════════════════════════════════════════════════════════════════════════════════════
#  3 Population ----
# ══════════════════════════════════════════════════════════════════════════════════════

cat("\n── 3. Population ──\n")

pop <- pop_files |>
  map(
    ~ {
      yr <- str_extract(basename(.x), "\\d{4}")
      src <- rast(.x)
      fact <- max(round(c(
        GRID_RES / res(src)[1] / 10,
        GRID_RES / res(src)[2] / 10
      )))
      if (fact > 1) {
        src <- aggregate(src, fact = fact, fun = "sum", na.rm = TRUE)
      }
      r <- if (same.crs(src, template)) {
        resample(src, template, method = "sum")
      } else {
        project(src, template, method = "sum")
      }
      setNames(r, map_year(yr))
    }
  ) |>
  reduce(c) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  mutate(across(x:y, ~ round(.x, 3) |> as.character()))

write_xlsx(
  pop,
  file.path(OUT_DIR, str_glue("GridPop_instance_{OUT_SUFFIX}.xlsx"))
)
cat(str_glue("  → {nrow(pop)} grids, {ncol(pop) - 2} scenarios\n"))

# ══════════════════════════════════════════════════════════════════════════════════════
#  4 Mortality rate ----
# ══════════════════════════════════════════════════════════════════════════════════════

cat("\n── 4. Mortality rate ──\n")

csv_files <- list.files(
  MORT_RAW_DIR,
  pattern = "\\.(csv|zip)$",
  full.names = TRUE
)

mortrate <- map(
  csv_files,
  ~ {
    read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")) |>
      filter(year %in% names(YEAR_MAP)) |>
      mutate(
        endpoint = str_replace_all(cause_name, map_cause),
        agegroup = parse_age(age_name),
        mortrate = as.numeric(val)
      ) |>
      na.omit() |>
      select(
        domain = location_name,
        year,
        agegroup,
        endpoint,
        mortrate
      )
  }
) |>
  list_rbind()

# llm_join: match grid country names to IHME location names
cat(str_glue(
  "  llm_join: {length(unique(grid_info$Country))} grid countries",
  " → {length(unique(mortrate$domain))} IHME locations ...\n"
))

mortality_country_map <- read_or_build_country_map(
  grid_info,
  mortrate,
  key1 = "Country",
  key2 = "domain",
  cache_path = "./Data/country_map_mortality.csv"
)

missing_countries <- setdiff(mortrate$domain, mortality_country_map$domain)
cat(str_glue(
  "  {length(missing_countries)} countries dropped due to grid resolution: ",
  "{str_c(sort(missing_countries), collapse = ', ')}.\n"
))

mortrate_val <- mortrate |>
  pivot_wider(names_from = "endpoint", values_from = "mortrate") |>
  mutate(`NCD+LRI` = NCD + LRI, year = map_year(year)) |>
  pivot_wider(names_from = agegroup, values_from = any_of(unname(map_cause))) |>
  left_join(mortality_country_map) |>
  filter(!is.na(Country)) |>
  select(-domain) |>
  pivot_longer(
    cols = -c(year, Country),
    names_to = c("endpoint", "agegroup"),
    names_sep = "_",
    values_to = "mortrate"
  ) |>
  filter(is.finite(mortrate)) |>
  mutate(mortrate = replace_na(mortrate, 0)) |>
  pivot_wider(
    names_from = "year",
    values_from = "mortrate"
  )

write_xlsx(
  mortrate_val,
  file.path(OUT_DIR, str_glue("GBD_mortality_instance_{OUT_SUFFIX}.xlsx"))
)
cat(str_glue(
  "  → {nrow(mortrate_val)} rows, {ncol(mortrate_val) - 3} scenarios\n"
))

# ══════════════════════════════════════════════════════════════════════════════════════
#  5 Age structure ----
# ══════════════════════════════════════════════════════════════════════════════════════

cat("\n── 5. Age structure ──\n")
pop_year_map <- c(
  "2015" = "base2015",
  "2019" = "SSP1-Baseline_2030"
)

year_map_matches <- function(year_map, years, scenarios) {
  identical(
    unname(year_map[as.character(years)]),
    scenarios
  )
}
if (!year_map_matches(
  YEAR_MAP,
  SCENARIO_YEAR_SPEC$mortality_year,
  SCENARIO_YEAR_SPEC$scenario
)) {
  stop("YEAR_MAP does not match SCENARIO_YEAR_SPEC mortality years.")
}
if (!year_map_matches(
  YEAR_MAP,
  SCENARIO_YEAR_SPEC$population_year,
  SCENARIO_YEAR_SPEC$scenario
)) {
  stop("YEAR_MAP does not match SCENARIO_YEAR_SPEC population years.")
}
if (!year_map_matches(
  pop_year_map,
  SCENARIO_YEAR_SPEC$age_year,
  SCENARIO_YEAR_SPEC$scenario
)) {
  stop("pop_year_map does not match SCENARIO_YEAR_SPEC age years.")
}

# fmt: skip
agegroups <- c(
  "Under 5",  "5 to 9",  "10 to 14",  "15 to 19",  "20 to 24",  
  "25 to 29",  "30 to 34",  "35 to 39",  "45 to 49",  "40 to 44",  
  "50 to 54",  "55 to 59",  "60 to 64",  "65 to 69",  "70 to 74",  
  "75 to 79",  "80 to 84",  "85 to 89",  "90 to 94",  "95 plus"
)

agestruc <- list.files(
  AGESTRUC_RAW_DIR,
  pattern = "(csv|zip)$",
  full.names = TRUE
) |>
  map(\(x) {
    read_csv(x, show_col_types = FALSE, col_types = cols(.default = "c")) |>
      filter(
        sex_name == "both",
        year_id %in% c(names(pop_year_map)),
        age_group_name %in% agegroups
      )
  }) |>
  list_rbind() |>
  mutate(
    agegroup = parse_age(age_group_name),
    year = recode(year_id, !!!pop_year_map),
    pop = as.numeric(val),
  ) |>
  select(domain = location_name, location_id, year, agegroup, pop)

cat(str_glue("  llm_join: matching grid countries to IHME locations ...\n"))

agegroup_country_map <- read_or_build_country_map(
  grid_info,
  agestruc,
  key1 = "Country",
  key2 = "domain",
  cache_path = "./Data/country_map_age.csv"
)

agestruc_prop <- agestruc |>
  pivot_wider(
    names_from = c(year, agegroup),
    names_sep = "@",
    values_from = pop
  ) |>
  filter(!(domain == "Georgia" & location_id == "533")) |>
  left_join(agegroup_country_map) |>
  filter(!is.na(Country)) |>
  select(-location_id, -domain) |>
  pivot_longer(
    cols = matches("\\d{1,2}$"),
    names_to = c("year", "agegroup"),
    names_sep = "@",
    values_to = "pop"
  ) |>
  group_by(year, Country) |>
  mutate(agestruc = prop.table(pop), .keep = "unused") |>
  ungroup() |>
  pivot_wider(names_from = "year", values_from = "agestruc")

write_xlsx(
  agestruc_prop,
  file.path(OUT_DIR, str_glue("GBD_agestructure_instance_{OUT_SUFFIX}.xlsx"))
)

cat(str_glue(
  "  → {nrow(agestruc_prop)} rows, {ncol(agestruc_prop) - 2} scenarios\n"
))

# ══════════════════════════════════════════════════════════════════════════════════════

# Write the instance manifest used by HealthBurdenCalc.R and DrivingFactors.R.
instance_files <- list(
  Grids = str_glue("./Data/Grid_info_instance_{OUT_SUFFIX}.xlsx"),
  Pop = str_glue("./Data/GridPop_instance_{OUT_SUFFIX}.xlsx"),
  Conc_real = str_glue("./Data/GridPM25_instance_{OUT_SUFFIX}.xlsx"),
  MortRate = str_glue("./Data/GBD_mortality_instance_{OUT_SUFFIX}.xlsx"),
  AgeGroup = str_glue("./Data/GBD_agestructure_instance_{OUT_SUFFIX}.xlsx")
)
missing_outputs <- instance_files[!file.exists(unlist(instance_files))]
if (length(missing_outputs) > 0) {
  stop(
    "Instance build did not produce: ",
    paste(unlist(missing_outputs), collapse = ", ")
  )
}

manifest <- list(
  suffix = OUT_SUFFIX,
  created = format(Sys.Date(), "%Y-%m-%d"),
  mixed_scenario_years = ALLOW_MIXED_SCENARIO_YEARS,
  scenario_years = SCENARIO_YEAR_SPEC,
  files = instance_files
)
write_json(
  manifest,
  file.path(OUT_DIR, "instance_manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("\nDone. Instance files → ", OUT_DIR, "\n", sep = "")
cat("Manifest → ", file.path(OUT_DIR, "instance_manifest.json"), "\n", sep = "")
cat(
  "Counterfactual concentration is not generated. Supply an approved ",
  "Conc_cf file separately and register it in the manifest.\n",
  sep = ""
)
