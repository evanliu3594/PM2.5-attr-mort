suppressPackageStartupMessages({
  library(testthat)
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(jsonlite)
})

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
setwd(project_root)

source("Code/model.R", encoding = "UTF8")
source("Code/data.R", encoding = "UTF8")
source("Code/mortality.R", encoding = "UTF8")
source("Code/uncertainty.R", encoding = "UTF8")
source("Code/aggregation.R", encoding = "UTF8")

reset_core_globals <- function() {
  assign(".RR_std_tbl", NULL, envir = globalenv())
  assign(".DGT_CONC", 1, envir = globalenv())
  assign(".CONC_UNIT", NULL, envir = globalenv())
  assign(".RR_meta", list(), envir = globalenv())
  assign(".PWRR_branch", "branch", envir = globalenv())
  invisible(TRUE)
}

rr_fixture <- function(mean_up = 1.5, mean_low = 1.1) {
  expand_grid(
    concentration = c("0", "10"),
    endpoint = c("copd", "ihd"),
    agegroup = "25",
    CI = c("MEAN", "UP", "LOW")
  ) |>
    mutate(
      RR = case_when(
        concentration == "0" ~ 1,
        CI == "MEAN" ~ 1.2,
        CI == "UP" ~ mean_up,
        CI == "LOW" ~ mean_low
      )
    )
}

core_inputs <- function(include_ihd_b = TRUE) {
  grids <- tibble(
    x = c("1", "1", "1", "1"),
    y = c("1", "2", "3", "4"),
    Country = c("A", "A", "B", "B"),
    Province = c("P1", "P2", "P3", "P4")
  )
  conc <- tibble(
    x = c("1", "1", "1", "1"),
    y = c("1", "2", "3", "4"),
    concentration = c("0", "10", "0", "10")
  )
  pop <- tibble(
    x = c("1", "1", "1", "1"),
    y = c("1", "2", "3", "4"),
    Pop = c(100, 100, 100, 100)
  )
  ag <- tibble(
    domain = c("A", "B"),
    agegroup = "25",
    AgeStruc = c(0.5, 0.5)
  )
  mrate <- tibble(
    domain = c("A", "A", "B", if (include_ihd_b) "B"),
    endpoint = c("copd", "ihd", "copd", if (include_ihd_b) "ihd"),
    agegroup = "25",
    MortRate = c(100, 100, 100, if (include_ihd_b) 100)
  )
  list(grids = grids, conc = conc, pop = pop, ag = ag, mrate = mrate)
}

set_rr_fixture <- function(mean_up = 1.5, mean_low = 1.1) {
  reset_core_globals()
  assign(".RR_std_tbl", rr_fixture(mean_up, mean_low), envir = globalenv())
  invisible(TRUE)
}

write_input_fixture <- function(dir, duplicate_grid = FALSE) {
  inputs <- core_inputs()
  grids <- inputs$grids |>
    select(x, y, Country)
  if (duplicate_grid) {
    grids <- bind_rows(grids, grids[1, ])
  }
  readr::write_csv(grids, file.path(dir, "grids.csv"))
  readr::write_csv(
    inputs$conc |> rename(base2015 = concentration),
    file.path(dir, "conc.csv")
  )
  readr::write_csv(
    inputs$pop |> rename(base2015 = Pop),
    file.path(dir, "pop.csv")
  )
  readr::write_csv(
    inputs$mrate |> rename(base2015 = MortRate),
    file.path(dir, "mort.csv")
  )
  readr::write_csv(
    inputs$ag |> rename(base2015 = AgeStruc),
    file.path(dir, "age.csv")
  )
  list(
    Grids = file.path(dir, "grids.csv"),
    Pop = file.path(dir, "pop.csv"),
    Conc_real = file.path(dir, "conc.csv"),
    MortRate = file.path(dir, "mort.csv"),
    AgeGroup = file.path(dir, "age.csv")
  )
}
