library(testthat)

context("summarize combat_events")

test_that("summarizes combat_events", {
  log <- c("1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 500",
           "1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000",
           "1468470478861786 [Damage Out] Captain Planet's Boxing damaged Xi-Rho for 1000")
  
  events <- parse_combat(log)
  summ_events <- summary(events)
})
