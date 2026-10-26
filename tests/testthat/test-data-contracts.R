setwd(normalizePath(file.path(testthat::test_path(), "..", "..")))

test_that("coordinate names are case-insensitive", {
  df <- tibble(lOn = 1, LaT = 2, value = 3)
  out <- normalize_coords(df)
  expect_equal(names(out), c("x", "y", "value"))
  expect_equal(out$x, 1)
  expect_equal(out$y, 2)
})

test_that("ambiguous coordinate columns abort", {
  df <- tibble(lon = 1, Longitude = 2, lat = 3)
  expect_error(normalize_coords(df), "Ambiguous")
})

test_that("scenario columns use names, not positions", {
  df <- tibble(base2015 = 1, x = "1", y = "2", future = 3)
  expect_equal(get_scenarios(df), c("base2015", "future"))
})

test_that("read_files rejects duplicate grid keys", {
  reset_core_globals()
  assign(".CR_Model", "TEST", envir = globalenv())
  dir <- withr::local_tempdir()
  paths <- write_input_fixture(dir, duplicate_grid = TRUE)
  expect_error(
    do.call(read_files, c(paths, list(dgt_grid = 2))),
    "duplicated key"
  )
})

test_that("explicit missing Conc_cf aborts and omitted Conc_cf stays NULL", {
  reset_core_globals()
  assign(".CR_Model", "TEST", envir = globalenv())
  dir <- withr::local_tempdir()
  paths <- write_input_fixture(dir, duplicate_grid = FALSE)

  expect_error(
    do.call(
      read_files,
      c(paths, list(Conc_cf = file.path(dir, "missing.xlsx"), dgt_grid = 2))
    ),
    "Conc_cf file not found"
  )

  expect_no_error(
    do.call(read_files, c(paths, list(Conc_cf = NULL, dgt_grid = 2)))
  )
  expect_null(Conc_cf)
})

test_that("instance manifest resolves existing files", {
  files <- resolve_instance_files()
  expect_true(all(c("Grids", "Pop", "Conc_real", "MortRate", "AgeGroup") %in% names(files)))
  expect_true(all(file.exists(unlist(files, use.names = FALSE))))
})

test_that("read_files rejects a concentration unit that conflicts with the model", {
  reset_core_globals()
  assign(".CR_Model", "TEST", envir = globalenv())
  assign(".RR_std_tbl", NULL, envir = globalenv())
  assign(".RR_meta", list(conc_unit = "ppb"), envir = globalenv())
  dir <- withr::local_tempdir()
  paths <- write_input_fixture(dir, duplicate_grid = FALSE)

  expect_error(
    do.call(read_files, c(paths, list(dgt_grid = 2, conc_unit = "ug/m3"))),
    "unit mismatch"
  )
})

test_that("RR table precision is recorded and enforced", {
  reset_core_globals()
  set_Model("NCD+LRI", dgt_conc = 2)
  expect_equal(attr(.RR_std_tbl, "dgt_conc"), 2)
  expect_equal(.DGT_CONC, 2)
  expect_true(all(str_detect(.RR_std_tbl$concentration, "^[0-9]+([.][0-9]{1,2})?$")))
})
