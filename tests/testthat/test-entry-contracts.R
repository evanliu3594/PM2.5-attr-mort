setwd(normalizePath(file.path(testthat::test_path(), "..", "..")))

test_that("entry scripts use the instance manifest", {
  health <- readLines("HealthBurdenCalc.R", warn = FALSE)
  drivers <- readLines("DrivingFactors.R", warn = FALSE)

  expect_true(any(str_detect(health, "resolve_instance_files\\(\\)")))
  expect_true(any(str_detect(drivers, "resolve_instance_files\\(\\)")))
  expect_false(any(str_detect(health, "Grid_info_instance_260617")))
  expect_false(any(str_detect(drivers, "Grid_info_instance_260617")))
})

test_that("DrivingFactors does not request a missing counterfactual file", {
  drivers <- readLines("DrivingFactors.R", warn = FALSE)
  expect_false(any(str_detect(drivers, "GridPM25_cf_instance")))
})

test_that("HealthBurdenCalc writes aggregation outputs by default", {
  health <- readLines("HealthBurdenCalc.R", warn = FALSE)
  expect_true(any(str_detect(health, "WRITE_RESULTS <- TRUE")))
  expect_true(any(str_detect(health, "write = TRUE")))
})

test_that("HealthBurdenCalc uses scenario names and an explicit PM2.5 unit", {
  health <- readLines("HealthBurdenCalc.R", warn = FALSE)
  expect_true(any(str_detect(health, "get_scenarios\\(\\)")))
  expect_true(any(str_detect(health, 'conc_unit = "ug/m3"')))
  expect_false(any(str_detect(health, "names\\(Conc_real\\)\\[c\\(-1:-2\\)\\]")))
})
