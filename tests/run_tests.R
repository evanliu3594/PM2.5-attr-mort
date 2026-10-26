#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(testthat))

root <- normalizePath(".")
setwd(root)
test_dir("tests/testthat", env = globalenv(), reporter = "summary")
