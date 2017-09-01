library(testthat)

context("parse_log")

test_that("parse_log matches pattern", {
  log <- c("1 a", "", "3 c")
  events <- parse_log(log, "^(?<time>[0-9]) (?<data>.+)")
  expect_equal(attr(events, "matched"), c(1, 3))
})

test_that("parse_log fails to match pattern", {
  events <- parse_log("a", "(?<time>[0-9])")
  expect_true(nrow(events) == 0)
  expect_true(length(attr(events, "matched")) == 0)
})

test_that("parse_log sets class", {
  events <- parse_log("", "(?<time>[0-9])", class="test_events")
  expect_s3_class(events, "test_events")
})

test_that("parse_log handles timestamps", {
  timestamp <- strptime("12/21/2012 03:14:15.926", "%m/%d/%Y %H:%M:%OS")
  usec_time <- as.character(as.numeric(timestamp) * 1e6)
  events <- parse_log(usec_time, "(?<time>[0-9]{16})", time_digits=3)
  expect_equal(events$time, timestamp)
})

context("parse_combat")

test_that("parse_combat parses damage out/in", {
  log <- c("1468470478861786 [Damage Out] Captain Planet's Ranged Attack damaged Xi-Rho for 1457",
           "1468470478861786 [Damage In] Captain Planet's Ranged Attack damaged Xi-Rho for 1457")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Captain Planet"))
  expect_true(all(events$target == "Xi-Rho"))
  expect_true(all(events$ability == "Ranged Attack"))
  expect_true(all(events$value == 1457))
  expect_true(all(events$type == "damage"))
  expect_true(all(events$crit == FALSE))
})

test_that("parse_combat parses crit damage", {
  log <- "1468470478861786 [Damage Out] Captain Planet's Ranged Attack critically damaged Xi-Rho for 1457"
  events <- parse_combat(log)
  expect_equal(events$crit, TRUE)
})

test_that("parse_combat parses healing out/in", {
  log <- c("1468470478861786 [Healing Out] Captain Planet's Metabolism healed Xi-Rho for 2555",
           "1468470478861786 [Healing In] Captain Planet's Metabolism healed Xi-Rho for 2555")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Captain Planet"))
  expect_true(all(events$target == "Xi-Rho"))
  expect_true(all(events$ability == "Metabolism"))
  expect_true(all(events$value == 2555))
  expect_true(all(events$type == "healing"))
  expect_true(all(events$crit == FALSE))
})

test_that("parse_combat parses power out/in", {
  log <- c("1468470478861786 [Power Out] Captain Planet's Soder Cola Extreme healed Xi-Rho for 625 Power",
           "1468470478861786 [Power In] Captain Planet's Soder Cola Extreme healed Xi-Rho for 625 Power")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Captain Planet"))
  expect_true(all(events$target == "Xi-Rho"))
  expect_true(all(events$ability == "Soder Cola Extreme"))
  expect_true(all(events$value == 625))
  expect_true(all(events$type == "power"))
  expect_true(all(events$crit == FALSE))
})

test_that("parse_combat parses absorb out/in", {
  log <- c("1468470478861786 [Combat Out] Brick's Jackhammer absorbed 901",
           "1468470478861786 [Combat In] Brick's Jackhammer absorbed 901")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Brick"))
  expect_true(all(events$target == ""))
  expect_true(all(events$ability == "Jackhammer"))
  expect_true(all(events$value == 901))
  expect_true(all(events$type == "absorb"))
  expect_true(all(events$crit == FALSE))
})

test_that("parse_combat parses supercharge out/in", {
  log <- c("1468470478861786 [Supercharge Out] Captain Planet's Recovery healed Xi-Rho for 625 Supercharge",
           "1468470478861786 [Supercharge In] Captain Planet's Recovery healed Xi-Rho for 625 Supercharge")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Captain Planet"))
  expect_true(all(events$target == "Xi-Rho"))
  expect_true(all(events$ability == "Recovery"))
  expect_true(all(events$value == 625))
  expect_true(all(events$type == "supercharge"))
  expect_true(all(events$crit == FALSE))
})

test_that("parse_combat handles possessive apostrophes", {
  log <- "1468470478861786 [Damage Out] Thanatos' Ranged Attack damaged Xi-Rho for 1457"
  events <- parse_combat(log)
  expect_true(events$source == "Thanatos")
})

test_that("parse_combat handles alphanumeric names", {
  log <- "1468470478861786 [Damage Out] Pengbot Maximus 2.0's Ranged Attack damaged Xi-Rho for 1457"
  events <- parse_combat(log)
  expect_true(events$source == "Pengbot Maximus 2.0")
})

test_that("parse_combat handles names with symbols", {
  log <- "1468470478861786 [Damage Out] ®Captain®'s Ranged Attack damaged Xi-Rho¥ for 1457"
  events <- parse_combat(log)
  expect_true(events$source == "®Captain®")
  expect_true(events$target == "Xi-Rho¥")
})

test_that("parse_combat handles lines with multiple apostrophes", {
  log <- "1468470478861786 [Damage Out] Captain Planet's Zeus's Heavenly Light damaged Xi-Rho for 1400"
  events <- parse_combat(log)
  expect_true(events$source == "Captain Planet")
  expect_true(events$ability == "Zeus's Heavenly Light")
})

test_that("parse_combat handles whitespace names", {
  log <- "1468470478861786 [Damage In]  's Form Change damaged Johnny Bravo for 8229"
  events <- parse_combat(log)
  expect_true(events$source == " ")
  expect_true(events$ability == "Form Change")
})

test_that("parse_combat fixes lowercase names that were capitalized at the beginning", {
  log <- c("1468470478861786 [Damage Out] Captain planet's Ranged Attack damaged captain planet for 625",
           "1468470478861786 [Healing Out] Captain planet's Recovery healed Xi-Rho for 625")
  events <- parse_combat(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$source == "captain planet"))
})

test_that("parse_combat does not parse damage transfer events", {
  log <- c("1468470478861786 [Damage Out] Captain Planet transferred Swamp Thing's Launching Uppercut damaged Captain Planet for 230 to Brick",
           "1468470478861786 [Damage Out] Captain Planet transferred 244 damage to someone else")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_combat does not parse non-combat events", {
  log <- c("",
           "14614698174036212314119213",
           "1461469817403625 [Cash] Captain Planet gained 50 cash",
           "1461469037770079 [Items] Captain Planet received 1 The Observations Journal of Jeremiah Arkham")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_combat does not parse misc combat events", {
  log <- c("1462481849920727 [Combat Out] Invader's Nanowire Collar affected Captain Planet",
           "1461469757782822 [Combat Out] Gloves of Omnipotence (Elite) affected Captain Planet",
           "1468222461179675 [Combat Out] Captain Planet summoned a pet using ability Summon: Crystal Golem",
           "1468469302096086 [Combat Out] Captain's Invisibility renders Captain stealthed",
           "1468469302096295 [Combat Out] Captain's Invisibility renders Captain invisible",
           "1468469302099848 [Combat Out] Captain's Invisibility changed Captain into Invisibility",
           "1466401906719227 [Combat Out] Captain's Psychic Empowerment renders Captain visible")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_combat does not parse bugged combat events", {
  log <- c("1462564291569652 [Damage Out] Killer Bee [*action*] Captain's Warped Reality",
           "1468222452190737 [Damage In] Killer Bee [*action*] Captain Planet's Standard Cross")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_combat does not parse negative power outs when summoning Crystal or Fury", {
  skip("letting this case pass for now... the log does not handle negative values well")
  log <- c("1468222462381855 [Power Out] Crystal's [*ability*] damaged Crystal for 1000 Power",
           "1468222462381855 [Power Out] Fury's [*ability*] damaged Fury for 1000 Power")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_combat handles empty logs", {
  log <- c("")
  events <- parse_combat(log)
  expect_true(nrow(events) == 0)
  expect_true(inherits(events, "combat_events"))
})

context("parse_summary")

test_that("parse_summary parses summary events", {
  log <- c("1468470478861786 [Summary] Damage [30.0s] 702/s - 21058 total - 59 hits (1920 max) - 16 (27.1%) crits - 1 target",
           "1468470478861786 [Summary] Healing [30.0s] 702/s - 21058 total - 59 hits (1920 max) - 16 (27.1%) crits - 1 target",
           "1468470478861786 [Summary] Power [30.0s] 702/s - 21058 total - 59 hits (1920 max) - 16 (27.1%) crits - 1 target")
  events <- parse_summary(log)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$type == c("damage", "healing", "power")))
  expect_true(all(events$interval == 30))
  expect_true(all(events$xps == 702))
  expect_true(all(events$total == 21058))
  expect_true(all(events$hits == 59))
  expect_true(all(events$max == 1920))
  expect_true(all(events$crits == 16))
  expect_true(all(events$crit_pct == 0.271))
  expect_true(all(events$targets == 1))
})

test_that("parse_summary handles a single hit", {
  log <- "1468470478861786 [Summary] Damage [30.0s] 1/s - 30 total - 1 hit (55 max) - 0 (0.0%) crits - 3 targets"
  events <- parse_summary(log)
  expect_true(events$hits == 1)
})

test_that("parse_summary handles multiple targets", {
  log <- "1468470478861786 [Summary] Damage [30.0s] 1/s - 30 total - 3 hits (55 max) - 0 (0.0%) crits - 3 targets"
  events <- parse_summary(log)
  expect_true(events$targets == 3)
})

test_that("parse_summary handles decimal time intervals", {
  log <- "1468470478861786 [Summary] Damage [4.3s] 1/s - 30 total - 3 hits (55 max) - 0 (0.0%) crits - 3 targets"
  events <- parse_summary(log)
  expect_true(events$interval == 4.3)
})

test_that("parse_summary handles zeroes", {
  log <- "1468470478861786 [Summary] Damage [0.0s] 0/s - 0 total - 0 hits (0 max) - 0 (0.0%) crits - 0 targets"
  events <- parse_summary(log)
  expect_true(events$interval == 0)
  expect_true(events$xps == 0)
  expect_true(events$total == 0)
  expect_true(events$hits == 0)
  expect_true(events$max == 0)
  expect_true(events$crits == 0)
  expect_true(events$crit_pct == 0)
  expect_true(events$targets == 0)
})

test_that("parse_summary handles zero time bug", {
  log <- "0 [Summary] Damage [30.0s] 702/s - 21058 total - 59 hits (1920 max) - 16 (27.1%) crits - 1 target"
  events <- parse_summary(log)
  expect_true(events$time == 0)
  expect_true(events$type == "damage")
  expect_true(events$interval == 30)
  expect_true(events$xps == 702)
  expect_true(events$total == 21058)
  expect_true(events$hits == 59)
  expect_true(events$max == 1920)
  expect_true(events$crits == 16)
  expect_true(events$crit_pct == 0.271)
  expect_true(events$targets == 1)
})

test_that("parse_summary handles empty logs", {
  log <- c("")
  events <- parse_summary(log)
  expect_true(nrow(events) == 0)
  expect_true(inherits(events, "parser_summary"))
})

context("parse_crowd_control")

test_parse_crowd_control <- function(string, effect, ability="Stomp Smash") {
  log <- c("1468470478861786 [Combat Out] Donna Troy's %s %s Captain Planet",
           "1468470478861786 [Combat In] Donna Troy's %s %s Captain Planet")
  log <- sprintf(log, ability, string)
  events <- parse_crowd_control(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Donna Troy"))
  expect_true(all(events$ability == ability))
  expect_true(all(events$effect == effect))
  expect_true(all(events$target == "Captain Planet"))
}

test_that("parse_crowd_control parses knockdown", {
  test_parse_crowd_control("knocked down", "knockdown")
})

test_that("parse_crowd_control parses juggle", {
  test_parse_crowd_control("juggled", "juggle")
})

test_that("parse_crowd_control parses stun", {
  test_parse_crowd_control("stunned", "stun")
})

test_that("parse_crowd_control parses pull", {
  test_parse_crowd_control("pulled toward", "pull")
})

test_that("parse_crowd_control parses push", {
  test_parse_crowd_control("pushed", "push")
})

test_that("parse_crowd_control parses grounding", {
  test_parse_crowd_control("grounded", "grounding")
})

test_that("parse_crowd_control parses encasement", {
  test_parse_crowd_control("encased", "encasement")
})

test_that("parse_crowd_control parses snare", {
  test_parse_crowd_control("snared", "snare")
})

test_that("parse_crowd_control parses root", {
  test_parse_crowd_control("rooted", "root")
})

test_that("parse_crowd_control parses knockback", {
  test_parse_crowd_control("knocked back", "knockback")
})

test_that("parse_crowd_control parses counter", {
  test_parse_crowd_control("suppressed", "counter", ability="Stomp Smash")
})

test_that("parse_crowd_control parses suppression", {
  test_parse_crowd_control("suppressed", "suppression", ability="Time Bomb")
  test_parse_crowd_control("suppressed", "suppression", ability="Time Shift")
  test_parse_crowd_control("suppressed", "suppression", ability="Temporal Vortex")
})

test_that("parse_crowd_control does not parse impulse", {
  log <- "1468470478861786 [Combat Out] Donna Troy's Ranged Attack impulsed Science Police Patrol Car"
  events <- parse_crowd_control(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_crowd_control handles empty logs", {
  log <- c("")
  events <- parse_crowd_control(log)
  expect_true(nrow(events) == 0)
  expect_true(inherits(events, "crowd_control_events"))
})

context("parse_knockout")

test_that("parse_knockout parses knockouts", {
  log <- c("1468470478861786 [Damage Out] Captain Planet knocked out Swamp Thing",
           "1468470478861786 [Damage In] Captain Planet knocked out Swamp Thing")
  events <- parse_knockout(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "Captain Planet"))
  expect_true(all(events$target == "Swamp Thing"))
})

test_that("parse_knockout ignores Restoration Barrel knockouts", {
  # TODO test that other object knockouts aren't counted
  log <- c("1468470478861786 [Damage Out] Captain Planet knocked out Restoration Barrel",
           "1468470478861786 [Damage In] Captain Planet knocked out Restoration Barrel")
  events <- parse_knockout(log)
  expect_true(length(attr(events, "matched")) == 0)
  expect_true(nrow(events) == 0)
})

test_that("parse_knockout handles empty logs", {
  log <- c("")
  events <- parse_knockout(log)
  expect_true(nrow(events) == 0)
  expect_true(inherits(events, "knockout_events"))
})

context("parse_dodge")

test_that("parse_dodge parses dodges", {
  log <- c("1468470478861786 [Damage In] captain planet z dodged Malfunctioning Jor-El A.I.'s Spike Wave",
           "1468470478861786 [Damage Out] captain planet z dodged Malfunctioning Jor-El A.I.'s Spike Wave")
  events <- parse_dodge(log)
  expect_true(nrow(events) == 2)
  expect_true(all(events$time == 1468470478.862))
  expect_true(all(events$source == "captain planet z"))
  expect_true(all(events$target == "Malfunctioning Jor-El A.I."))
  expect_true(all(events$ability == "Spike Wave"))
})

test_that("parse_dodge does not parse evades", {
  log <- c("1468470478861786 [Damage In] captain planet z evaded Sparring Target's Restore",
           "1468470478861786 [Damage Out] captain planet z evaded Malfunctioning Jor-El A.I.'s Spike Wave")
  events <- parse_dodge(log)
  expect_true(nrow(events) == 0)
})

test_that("parse_dodge handles empty logs", {
  log <- c("")
  events <- parse_dodge(log)
  expect_true(nrow(events) == 0)
  expect_true(inherits(events, "dodge_events"))
})
