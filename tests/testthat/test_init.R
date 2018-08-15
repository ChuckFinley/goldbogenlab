library(goldbogenlab)
context("Tag initiation")

test_that("init_prh warns when timestamps are out of order", {
  prh0 <- PRH("mn180423-44", "44")
  expect_warning(prh1 <- init_prh(prh0))
})

test_that("Decimation requires sorted, gapless timestamps", {
  prh <- init_prh(PRH("mn180607-44", "44"))
  expect_error(prh_deci <- decimate_prh(prh))
})

test_that("fix_backskip fixes backwards time skips", {
  prh <- init_prh(PRH("mn180607-44", "44"))
  expect_message(prh_fix <- fix_backskip(prh))
  expect_false(is.unsorted(prh_fix@rawdata$datetimeUTC))
})

test_that("fix_gap won't fix data with backwards time skips", {
  prh <- init_prh(PRH("mn180607-44", "44"))
  expect_error(prh_fix <- fix_gap(prh))
})

test_that("fix_gap fixes gaps", {
  prh <- fix_backskip(init_prh(PRH("mn180607-44", "44")))
  expect_message(prh_fix <- fix_gap(prh))
  periods <- as.numeric(diff(prh_fix@rawdata$datetimeUTC), units = "secs")
  expected_period <- median(periods)
  tol_factor <- 100
  expect_equal(periods,
               expected_period,
               tolerance = expected_period / tol_factor)
})

test_that("Fixing data produces good decimation", {
  prh <- fix_gap(fix_backskip(init_prh(PRH("mn180607-44", "44"))))
  new_freq <- 10
  prh_deci <- decimate_prh(prh, new_freq)
  periods <- as.numeric(diff(prh_deci@rawdata$datetimeUTC), units = "secs")
  tol_factor <- 100
  expect_equal(periods, new_freq, tolerance = new_freq / 100)
})
