library(goldbogenlab)
library(testthat)
context("Apply calibrations")

# Loads and processes raw data from mn180607-44 for testion
new_freq <- 10
tagon <- as.POSIXct("2018-06-07 15:46:18", tz = "UTC")
tagoff <- as.POSIXct("2018-06-07 18:46:38", tz = "UTC")
suppressWarnings(prh <- init_prh("mn180607-44", "44") %>%
                   fix_backskip %>%
                   fix_gap %>%
                   decimate_prh(new_freq) %>%
                   trim_data(use_gui = FALSE,
                             tagon = tagon,
                             tagoff = tagoff) %>%
                   interp_gaps)

# Expect a numeric object falls within a range (given a tolerance)
expect_within <- function(object, expected, tolerance = 0) {
  stopifnot(is.numeric(object),
            is.numeric(expected) && length(expected) == 2,
            expected[2] > expected[1],
            is.numeric(tolerance) && (length(tolerance) == 1))
  percent_within <- sum(object >= expected[1] & object <= expected[2]) /
    length(object)
  expect_gte(percent_within, 1 - tolerance)
}

test_that("load_cal fails for bogus tagnums", {
  expect_error(load_cal("bogus"))
  expect_error(load_cal(44))
})

test_that("load_cal element names are correct", {
  cal <- load_cal("44")
  expect_named(cal, c("A", "acal", "aconst", "gcal", "gconst", "moffcal",
                      "moncal", "moffconst", "monconst", "Tcal",
                      "Tconst", "pcal", "pconst"))
})

test_that("bench_cal checks for trimmed, decimated data", {
  prh <- PRH("mn180607-44", "44")
  # Uninitiated PRH
  expect_error(bench_cal(prh))
  # Undecimated PRH
  suppressWarnings(prh <- init_prh("mn180607-44", "44"))
  expect_error(bench_cal(prh))
  # Untrimmed PRH
  suppressWarnings(prh <- prh %>% fix_backskip %>% fix_gap)
  expect_error(bench_cal(prh))
})

test_that("bench_cal fills in proper slots and fields", {
  prh <<- bench_cal(prh)
  expect_gt(nrow(prh@At), 0)
  expect_gt(nrow(prh@Gt), 0)
  expect_gt(nrow(prh@Mt), 0)
  expect_named(prh@data, c("t", "p", "camon"), ignore.order = TRUE)
})

test_that("bench_cal gets values into the right ballpark", {
  reasonableA <- c(-2, 2)
  reasonableM <- c(-50, 50)
  reasonableG <- c(-1, 1)

  tol <- 0.05
  expect_within(prh@At, reasonableA, tol)
  expect_within(prh@Mt, reasonableM, tol)
  expect_within(prh@Gt, reasonableG, tol)
})

# Test final calibration results (bench + in situ)
# Expected values
# oracle <- R.matlab::readMat("tests/testdata/cal_oracle.mat")
# expected_dt <- lubridate::ymd_hms(oracle$oracle.datetime,
#                                   tz = "Etc/GMT+7") %>%
#   lubridate::with_tz("UTC")
# i <- which(dplyr::between(prh@data$t, min(expected_dt), max(expected_dt)))
# expected_At <- oracle$oracle.At
# expected_Mt <- oracle$oracle.Mt
# expected_Gt <- oracle$oracle.Gt
#
# # Do observed values predict expected?
# bench_fit <- function(obs_mat, exp_mat, sensor) {
#   purrr::map_dfr(1:3, function(axis) {
#     x <- obs_mat[, axis]
#     y <- approx(expected_dt, exp_mat[, axis], xout = observed_dt)$y
#     bench_lm <- lm(y ~ x)
#     dplyr::tibble(sensor = sensor,
#                   axis = c("x", "y", "z")[axis],
#                   slope = coef(bench_lm)[2],
#                   r2 = summary(bench_lm)$r.squared)
#   })
# }
# observed <- list(observed_At, observed_Mt, observed_Gt)
# expected <- list(expected_At, expected_Mt, expected_Gt)
# result <- purrr::pmap_dfr(list(observed,
#                                expected,
#                                sensor = list("acc", "mag", "gyr")),
#                           bench_fit)
#
# compare_bench_cal <- function(sensor, axis) {
#   obs <- switch(sensor,
#                 acc = observed_At,
#                 mag = observed_Mt,
#                 gyr = observed_Gt)
#
#   exp <- switch(sensor,
#                 acc = expected_At,
#                 mag = expected_Mt,
#                 gyr = expected_Gt)
#
#   axis_i <- which(axis == c("x", "y", "z"))
#
#   data <- dplyr::tibble(t = observed_dt,
#                         obs = obs[, axis_i],
#                         exp = approx(expected_dt,
#                                      exp[, axis_i],
#                                      xout = t)$y)
#
#   xy_plot <- ggplot2::ggplot(data, ggplot2::aes(obs, exp)) +
#     ggplot2::geom_point() +
#     ggplot2::geom_smooth(method = "lm") +
#     ggplot2::labs(title = stringr::str_glue("{sensor} {axis}")) +
#     ggplot2::theme_classic()
#   ts_plot <- tidyr::gather(data, obs_exp, value, obs, exp) %>%
#     ggplot2::ggplot(ggplot2::aes(t, value, color = obs_exp)) +
#     ggplot2::geom_line(size = 0.5,
#                        alpha = 0.5) +
#     ggplot2::theme_classic()
#
#   gridExtra::grid.arrange(xy_plot, ts_plot, ncol = 1)
# }
