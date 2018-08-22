library(goldbogenlab)
library(testthat)
context("Tag initiation")

# Uses the following global variables to avoid lengthy operations:
#   prh <<- init_prh("mn180607-44", "44")
#   prh_backskip <<- fix_backskip(prh)
#   prh_gap <<- fix_gap(prh_backskip)
#   prh_deci <<- decimate_prh(prh_gap, new_freq)
#   prh_trim <<- trim_data(...)

# Utility functions for comparing dates
expect_date_gt <- function(object, expected) {
  expect_gt(as.numeric(object), as.numeric(expected))
}
expect_date_lt <- function(object, expected) {
  expect_lt(as.numeric(object), as.numeric(expected))
}

test_that("init_prh warns when timestamps are out of order", {
  expect_warning(prh <<- init_prh("mn180607-44", "44"))
})

test_that("Decimation requires sorted, gapless timestamps", {
  expect_error(decimate_prh(prh))
})

test_that("fix_gap won't fix data with backwards time skips", {
  expect_error(fix_gap(prh))
})

test_that("fix_backskip fixes backwards time skips", {
  expect_message(prh_backskip <<- fix_backskip(prh))
  expect_false(is.unsorted(prh_backskip@rawdata$datetimeUTC))
})

test_that("fix_gap fixes gaps", {
  expect_message(prh_gap <<- fix_gap(prh_backskip))
  periods <- as.numeric(diff(prh_gap@rawdata$datetimeUTC), units = "secs")
  freq <- round(1 / median(periods))
  expected_periods <- rep(1 / freq, length(periods))
  tol_factor <- 100
  expect_equal(periods,
               expected_periods,
               tolerance = expected_periods[1] / tol_factor)
})

test_that("fixing data produces good decimation", {
  new_freq <- 10
  prh_deci <<- decimate_prh(prh_gap, new_freq)
  expect_equal(prh_deci@freq, new_freq)
  periods <- as.numeric(diff(prh_deci@rawdata$datetimeUTC), units = "secs")
  expected_periods <- rep(1 / new_freq, length(periods))
  tol_factor <- 100
  expect_equal(periods, expected_periods, tolerance = new_freq / tol_factor)
})

test_that("trimming data sets PRH slots appropriately.", {
  old_nrow <- nrow(prh_deci@rawdata)
  prh_trim <- prh_deci %>%
    trim_data(use_gui = FALSE,
              tagon = as.POSIXct("2018-06-07 15:46:18", tz = "UTC"),
              tagoff = as.POSIXct("2018-06-07 18:46:38", tz = "UTC"))
  expect_date_gt(prh_trim@tagon, num_to_POSIX(0))
  expect_date_gt(prh_trim@tagoff, num_to_POSIX(0))
  expect_date_lt(prh_trim@tagon, prh_trim@tagoff)
  expect_lt(nrow(prh_trim@rawdata), old_nrow)
  expect_true(all(dplyr::between(prh_trim@rawdata$datetimeUTC,
                                 prh_trim@tagon,
                                 prh_trim@tagoff)))
})
