setwd(normalizePath(file.path(testthat::test_path(), "..", "..")))

test_that("duplicate input keys abort instead of duplicating deaths", {
  set_rr_fixture()
  inputs <- core_inputs()

  expect_error(
    Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = bind_rows(inputs$pop, inputs$pop[1, ]),
      CI = "MEAN",
      domain = "Country"
    ),
    "duplicated key"
  )

  expect_error(
    Mortality(
      Grids = bind_rows(inputs$grids, inputs$grids[1, ]),
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = inputs$pop,
      CI = "MEAN",
      domain = "Country"
    ),
    "duplicated key"
  )

  expect_error(
    Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = bind_rows(inputs$mrate, inputs$mrate[1, ]),
      pop = inputs$pop,
      CI = "MEAN",
      domain = "Country"
    ),
    "duplicated key"
  )
})

test_that("irrelevant metadata NA does not drop grids or alter PWRR", {
  set_rr_fixture()
  inputs <- core_inputs()
  base <- Mortality(
    Grids = inputs$grids,
    Conc_r = inputs$conc,
    ag = inputs$ag,
    mRate = inputs$mrate,
    pop = inputs$pop,
    CI = "MEAN",
    domain = "Country"
  )

  grids_na <- inputs$grids
  grids_na$Province[4] <- NA
  altered <- Mortality(
    Grids = grids_na,
    Conc_r = inputs$conc,
    ag = inputs$ag,
    mRate = inputs$mrate,
    pop = inputs$pop,
    CI = "MEAN",
    domain = "Country"
  )

  expect_equal(altered$x, base$x)
  expect_equal(altered$y, base$y)
  expect_equal(altered$copd_25_MEAN, base$copd_25_MEAN)
  expect_equal(altered$ihd_25_MEAN, base$ihd_25_MEAN)
})

test_that("single CI branches equal RANGE branches for both PWRR modes", {
  inputs <- core_inputs()

  for (mode in c("branch", "mean")) {
    set_rr_fixture()
    assign(".PWRR_branch", mode, envir = globalenv())

    range <- Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = inputs$pop,
      CI = "RANGE",
      domain = "Country"
    )
    up <- Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = inputs$pop,
      CI = "UP",
      domain = "Country"
    )
    low <- Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = inputs$pop,
      CI = "LOW",
      domain = "Country"
    )

    expect_equal(up$copd_25_UP, range$copd_25_UP)
    expect_equal(low$copd_25_LOW, range$copd_25_LOW)
  }
})

test_that("Mortality honours dgt_conc", {
  reset_core_globals()
  assign(".DGT_CONC", 2, envir = globalenv())
  rr <- expand_grid(
    concentration = c("0", "10.25"),
    CI = c("MEAN", "UP", "LOW")
  ) |>
    mutate(
      endpoint = "copd",
      agegroup = "25",
      RR = case_when(
        concentration == "0" ~ 1,
        CI == "MEAN" ~ 1.2,
        CI == "UP" ~ 1.3,
        CI == "LOW" ~ 1.1
      )
    )
  attr(rr, "dgt_conc") <- 2
  assign(".RR_std_tbl", rr, envir = globalenv())
  assign(".PWRR_branch", "branch", envir = globalenv())

  inputs <- core_inputs()
  conc <- tibble(
    x = c("1", "1"),
    y = c("1", "2"),
    concentration = c("0", "10.25")
  )
  pop <- tibble(x = c("1", "1"), y = c("1", "2"), Pop = c(100, 100))
  grids <- tibble(x = c("1", "1"), y = c("1", "2"), Country = "A")
  ag <- tibble(domain = "A", agegroup = "25", AgeStruc = 0.5)
  mrate <- tibble(
    domain = "A",
    endpoint = "copd",
    agegroup = "25",
    MortRate = 100
  )

  mort <- Mortality(
    Grids = grids,
    Conc_r = conc,
    ag = ag,
    mRate = mrate,
    pop = pop,
    CI = "MEAN",
    domain = "Country"
  )
  expect_equal(nrow(mort), 2)
  expect_true(all(is.finite(mort$copd_25_MEAN)))
  expect_true(mort$copd_25_MEAN[2] > 0)
})

test_that("precision mismatch between RR table and reader aborts", {
  reset_core_globals()
  assign(".DGT_CONC", 2, envir = globalenv())
  rr <- tibble(
    concentration = "0",
    endpoint = "copd",
    agegroup = "25",
    CI = "MEAN",
    RR = 1
  )
  attr(rr, "dgt_conc") <- 1
  assign(".RR_std_tbl", rr, envir = globalenv())

  inputs <- core_inputs()
  expect_error(
    Mortality(
      Grids = inputs$grids,
      Conc_r = inputs$conc,
      ag = inputs$ag,
      mRate = inputs$mrate,
      pop = inputs$pop,
      CI = "MEAN",
      domain = "Country"
    ),
    "precision"
  )
})
