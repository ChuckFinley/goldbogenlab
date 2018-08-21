library(goldbogenlab)
library(testthat)
context("Apply calibrations")

# Loads and processes raw data from mn180607-44 for testion
new_freq <- 10
prh <- init_prh(PRH("mn180607-44", "44")) %>%
  fix_backskip %>%
  fix_gap %>%
  decimate_prh(new_freq) %>%
  trim_data(use_gui = FALSE,
            tagon = as.POSIXct("2018-06-07 15:46:18", tz = "UTC"),
            tagoff = as.POSIXct("2018-06-07 18:46:38", tz = "UTC"))

test_that("load_cal fails for bogus tagnums", {
  expect_error(load_cal("bogus"))
  expect_error(load_cal(44))
})

test_that("load_cal element names are correct", {
  cal <- load_cal("44")
  expect_named(cal, c("A", "acal", "aconst", "gcal", "gconst", "mcaloff",
                      "mcalon", "mcalonconst", "mcaloffconst", "Tcal",
                      "Tconst", "pcal", "pconst"))
})

test_that("bench_cal checks for trimmed, decimated data", {
  prh <- PRH("mn180607-44", "44")
  # Uninitiated PRH
  expect_error(bench_cal(prh))
  # Undecimated PRH
  prh <- init_prh(prh)
  expect_error(bench_cal(prh))
  # Untrimmed PRH
  prh <- prh %>% fix_backskip %>% fix_gap
  expect_error(bench_cal(prh))
})

test_that("bench_cal fills in proper slots and fields", {
  prh <<- bench_cal(prh)
  expect_gt(nrow(prh@At), 0)
  expect_gt(nrow(prh@Gt), 0)
  expect_gt(nrow(prh@Mt), 0)
  expect_named(prh@data, c("t", "p", "camon"))
})

test_that("bench_cal correctly applies bench calibrations", {

})
