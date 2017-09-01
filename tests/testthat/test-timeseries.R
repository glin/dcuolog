library(testthat)

context("timeseries")

test_that("timeseries.combat_events handles events out of order", {
  log <- c("1468470479861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 2000",
           "1468470477861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 3555",
           "1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1457")
  events <- parse_combat(log)
  ts <- timeseries(events)
  expect_equal(ts$time, as.POSIXct(c(1468470478, 1468470479, 1468470480), origin = "1970-01-01"))
  expect_equal(ts$value, c(3555, 1457, 2000))
})

test_that("timeseries.combat_events fails with zero events", {
  events <- parse_combat("")
  expect_error(timeseries(events))
})

test_that("wma_gaussian fails when window is larger than the data", {
  expect_error(wma_gaussian(rep(1, 3), 4))
})

test_that("gaussian window fails for invalid window size", {
  for (size in c(-1, 0, 1)) {
    expect_error(gaussian_window(size))
  }
})
