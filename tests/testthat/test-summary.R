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

test_that("summarizes combat_events by ability", {
  log <- c("1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 500",
           "1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000")

  events <- parse_combat(log)
  summ_events <- summary(events, by = "ability")
})

test_that("summarizes combat_events with power metrics", {
  log <- c("1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 500",
           "1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000",
           "1468470478861786 [Power In] Captain Planet's Soder Cola Extreme healed Captain Planet for 500 Power",
           "1468470478861786 [Power In] Captain Planet's Soder Cola Extreme healed Captain Planet for 1000 Power")

  events <- parse_combat(log)
  summ_events <- summary(events, power_metrics = TRUE)
})

test_that("power_metrics only valid when grouping by source", {
  events <- parse_combat("")
  summary(events, by = "source", power_metrics = TRUE)
  for (group_by in list(c("source", "target"), c("source", "ability"), c("target", "ability"))) {
    expect_error(summary(events, by = group_by, power_metrics = TRUE))
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
  time <- as.POSIXct(0, origin = "1970-01-01")
  duration <- total_time(time)
  expect_true(is.na(duration))
  expect_type(duration, "double")
})

test_that("total_time handles zero observations", {
  time <- c()
  duration <- total_time(time)
  expect_true(is.na(duration))
  expect_type(duration, "double")
})

test_that("total_time handles multiple observations at the same point in time", {
  time <- rep(as.POSIXct(0, origin = "1970-01-01"), 4)
  duration <- total_time(time)
  expect_equal(duration, 0)
})

test_that("active_time works for a single instance of combat", {
  time <- c(1, 2, 4)
  duration <- active_time(time, window = 3)
  expect_true(duration == 3)
})

test_that("active_time works for multiple instances of combat", {
  time <- c(1, 2, 4, 10, 12, 14)
  duration <- active_time(time, window = 3)
  expect_true(duration == 7)
})

test_that("active_time handles a single observation", {
  time <- 1
  duration <- active_time(time)
  expect_true(is.na(duration))
  expect_type(duration, "double")
})

test_that("active_time handles zero observations", {
  time <- c()
  duration <- active_time(time)
  expect_true(is.na(duration))
  expect_type(duration, "double")
})

test_that("active_time handles multiple observations at the same point in time", {
  time <- c(1, 1, 1, 1)
  duration <- active_time(time, window = 3)
  expect_true(duration == 0)
})

test_that("active_time handles a single instance of combat exceeding the window", {
  time <- c(1, 100)
  duration <- active_time(time, window = 3)
  expect_true(duration == 99)
})

test_that("active_time handles multiple instances of combat exceeding the window", {
  time <- c(1, 100, 200, 400)
  duration <- active_time(time, window = 3)
  expect_true(duration == 199)
})
