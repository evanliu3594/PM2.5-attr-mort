setwd(normalizePath(file.path(testthat::test_path(), "..", "..")))

test_that("all builders parse under R 4.6.1", {
  builders <- c(
    "Code/build_CR.R",
    "Code/build_CR_MRBRT2023.R",
    "Code/build_CR_GBD2023_gas.R",
    "build_instance.R"
  )
  for (file in builders) {
    expect_no_error(parse(file))
  }
})

test_that("build_instance no longer generates random counterfactual data", {
  lines <- readLines("build_instance.R", warn = FALSE)
  expect_false(any(str_detect(lines, "rnorm\\(")))
  expect_false(any(str_detect(lines, "GridPM25_cf_instance_")))
})

test_that("RR config declares unit and range for every model", {
  cfg <- jsonlite::fromJSON(
    "Data/RR_std_config.json",
    simplifyVector = FALSE
  )
  for (model in names(cfg)) {
    expect_true(!is.null(cfg[[model]]$conc_unit), info = model)
    expect_equal(length(cfg[[model]]$conc_range), 2, info = model)
  }
})
