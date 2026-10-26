setwd(normalizePath(file.path(testthat::test_path(), "..", "..")))

test_that("aggregate_range preserves missing mortality as NA", {
  set_rr_fixture()
  inputs <- core_inputs(include_ihd_b = FALSE)
  assign("Grid_info", inputs$grids, envir = globalenv())
  mort <- Mortality(
    Grids = inputs$grids,
    Conc_r = inputs$conc,
    ag = inputs$ag,
    mRate = inputs$mrate,
    pop = inputs$pop,
    CI = "MEAN",
    domain = "Country"
  )

  expect_true(is.na(mort$ihd_25_MEAN[mort$y == "4"]))

  total <- suppressMessages(
    aggregate_range(list(s1 = mort), at = "Country", by = "total")[[1]]
  )
  all_groups <- suppressMessages(
    aggregate_range(list(s1 = mort), at = "Country", by = "all")[[1]]
  )

  expect_true(is.na(total$s1_MEAN[total$Country == "B"]))
  expect_true(is.na(
    all_groups$s1_MEAN[
      all_groups$Country == "B" & all_groups$endpoint == "ihd"
    ]
  ))
})

test_that("numeric Grid_info metadata never enters mortality totals", {
  set_rr_fixture()
  inputs <- core_inputs()
  inputs$grids <- inputs$grids |>
    mutate(grid_id = c(10, 20, 30, 40), area = c(1, 2, 3, 4))
  assign("Grid_info", inputs$grids, envir = globalenv())

  mort <- Mortality(
    Grids = inputs$grids,
    Conc_r = inputs$conc,
    ag = inputs$ag,
    mRate = inputs$mrate,
    pop = inputs$pop,
    CI = "MEAN",
    domain = "Country"
  )

  total <- suppressMessages(
    aggregate_range(list(s1 = mort), at = "Country", by = "total")[[1]]
  )
  grid_total <- suppressMessages(
    aggregate_range(list(s1 = mort), at = "grid", by = "total")[[1]]
  )

  expected_domain <- 2 * 100 * 0.5 * 100 * 0.2 / 1.1 / 1e5
  expect_equal(total$s1_MEAN[total$Country == "A"], expected_domain)
  expect_equal(total$s1_MEAN[total$Country == "B"], expected_domain)
  expect_false(any(c(10, 20, 30, 40) %in% unlist(total)))
  expected_grid <- c(0, expected_domain, 0, expected_domain)
  expect_equal(grid_total$s1_MEAN, expected_grid)
})

test_that("aggregate_range totals equal direct grid sums", {
  set_rr_fixture()
  inputs <- core_inputs()
  assign("Grid_info", inputs$grids, envir = globalenv())
  mort <- Mortality(
    Grids = inputs$grids,
    Conc_r = inputs$conc,
    ag = inputs$ag,
    mRate = inputs$mrate,
    pop = inputs$pop,
    CI = "MEAN",
    domain = "Country"
  )
  direct <- sum(mort$copd_25_MEAN) + sum(mort$ihd_25_MEAN)
  total <- suppressMessages(
    aggregate_range(list(s1 = mort), at = "Country", by = "total")[[1]]
  )
  expect_equal(sum(total$s1_MEAN), direct)
})

test_that("aggregate_sigma uses PWRR branch totals and keeps underscore domains", {
  set_rr_fixture(mean_up = 3, mean_low = 1.1)
  grids <- tibble(
    x = c("1", "1"),
    y = c("1", "2"),
    Country = "A_B"
  )
  conc <- tibble(
    x = c("1", "1"),
    y = c("1", "2"),
    base2015 = c(0, 20)
  )
  pop <- tibble(x = c("1", "1"), y = c("1", "2"), base2015 = c(100, 100))
  ag <- tibble(domain = "A_B", agegroup = "25", base2015 = 0.5)
  mrate <- tibble(
    domain = "A_B",
    endpoint = c("copd", "ihd"),
    agegroup = "25",
    base2015 = 200
  )

  assign("Grid_info", grids, envir = globalenv())
  assign("Conc_real", conc, envir = globalenv())
  assign("Pop", pop, envir = globalenv())
  assign("AgeGroup", ag, envir = globalenv())
  assign("MortRate", mrate, envir = globalenv())

  mean_grid <- Mortality_at("base2015", CI = "MEAN", domain = "Country")
  sigma <- suppressMessages(
    aggregate_sigma(
      list(base2015 = mean_grid),
      at = "Country",
      by = "total",
      write = FALSE
    )
  )

  baseline <- 2 * 200 * 0.5 * 200 / 1e5
  expected_mean <- baseline * (1 - 1 / 1.1)
  expected_up <- baseline * (1 - 1 / 2)
  expected_low <- baseline * (1 - 1 / 1.05)

  expect_equal(sigma$year, "base2015")
  expect_equal(sigma$Country, "A_B")
  expect_equal(sigma$total_MEAN, expected_mean)
  expect_equal(sigma$total_UP, expected_up)
  expect_equal(sigma$total_LOW, expected_low)

  sigma_conc <- suppressMessages(
    aggregate_sigma(
      list(base2015 = mean_grid),
      at = "Country",
      by = "total",
      write = FALSE,
      includeConc = TRUE,
      Conc_ERR = 10
    )
  )
  expect_equal(sigma_conc$total_MEAN, sigma$total_MEAN)
  expect_true(sigma_conc$total_UP >= sigma$total_UP)
  expect_true(sigma_conc$total_LOW <= sigma$total_LOW)
})

test_that("aggregate_sigma rejects unsupported breakdowns", {
  expect_error(
    aggregate_sigma("base2015", by = "endpoint", write = FALSE),
    "total"
  )
})

test_that("retired Uncertainty points to aggregate_sigma", {
  expect_error(Uncertainty(), "retired")
})
