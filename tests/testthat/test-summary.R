library(testthat)

context("summary.combat_events")

test_that("summarizes combat_events", {
  log <- c("1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 500",
           "1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000")
  
  events <- parse_combat(log)
  summ_events <- summary(events)
})

test_that("power_metrics not valid for types other than damage, power, healing, absorb", {
  events <- parse_combat("")
  for (type in c("damage_in", "healing_in", "power_in", "supercharge")) {
    expect_error(summary(events, type=type, power_metrics=TRUE))
  }
})

test_that("handles empty combat_events", {
  events <- parse_combat("")
  summ <- summary(events)
  expect_true(nrow(summ) == 0)
  expect_true(inherits(summ, "combat_summary"))
})

context("summary.parser_summary")

test_that("handles empty parser_summary", {
  events <- parse_summary("")
  summ <- summary(events)
  expect_true(nrow(summ) == 0)
  expect_true(inherits(summ, "parser_summary"))
})

context("summary.crowd_control_events")

test_that("handles empty crowd_control_events", {
  events <- parse_crowd_control("")
  summ <- summary(events)
  expect_true(nrow(summ) == 0)
  expect_true(inherits(summ, "crowd_control_summary"))
})

context("summary.knockout_events")

test_that("handles empty knockout_events", {
  events <- parse_knockout("")
  summ <- summary(events)
  expect_true(nrow(summ) == 0)
  expect_true(inherits(summ, "knockout_summary"))
})

context("summary.dodge_events")

test_that("handles empty dodge_events", {
  events <- parse_dodge("")
  summ <- summary(events)
  expect_true(nrow(summ) == 0)
  expect_true(inherits(summ, "dodge_summary"))
})

context("summary")

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

test_that("total_time handles multiple observations at the same point in time", {
  time_obs <- rep(as.POSIXct(0, origin="1970-01-01"), 4)
  time <- total_time(time_obs)
  expect_equal(time, 0)
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

test_that("active_time handles multiple observations at the same point in time", {
  time <- c(1, 1, 1, 1)
  combat_time <- active_time(time, window=3)
  expect_true(combat_time == 0)
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
