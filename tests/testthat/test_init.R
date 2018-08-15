library(goldbogenlab)
context("Tag initiation")

test_that("init_prh warns when timestamps are out of order (mn180423-44)", {
  prh0 <- PRH("mn180423-44", "44")
  expect_warning(prh1 <- init_prh(prh0))
  expect_equal(nrow(prh1@data), 0)
  expect_equal(nrow(prh1@rawdata), 57059)
})
