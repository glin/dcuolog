library(testthat)

context("metrics")

test_that("total_time handles a single observation", {
  time_obs <- as.POSIXct(0, origin="1970-01-01")
  time <- total_time(time_obs)
  expect_true(is.na(time))
})

test_that("total_time handles zero observations", {
  time_obs <- c()
  time <- total_time(time_obs)
  expect_true(is.na(time))
})

test_that("active_time works for a single instance of combat", {
  time <- c(1, 2, 4)
  combat_time <- active_time(time, window=3)
  expect_true(combat_time == 3)
})

test_that("active_time works for multiple instances of combat", {
  time <- c(1, 2, 4, 10, 12, 14)
  combat_time <- active_time(time, window=3)
  expect_true(combat_time == 7)
})

test_that("active_time handles a single observation", {
  time <- 1
  combat_time <- active_time(time)
  expect_true(is.na(combat_time))
})

test_that("active_time handles zero observations", {
  time <- c()
  combat_time <- active_time(time)
  expect_true(is.na(combat_time))
})

test_that("active_time handles a single instance of combat exceeding the window", {
  time <- c(1, 100)
  combat_time <- active_time(time, window=3)
  expect_true(combat_time == 99)
})

test_that("active_time handles multiple instances of combat exceeding the window", {
  time <- c(1, 100, 200, 400)
  combat_time <- active_time(time, window=3)
  expect_true(combat_time == 199)
})
