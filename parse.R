library(data.table)

summary.CombatLog <- function(x) {}
metrics.CombatLog <- function(x) {}
metrics.time.CombatLog <- function(x) {}

parse_dcuo <- function(log, file=NULL) {
  if (missing(log) && !is.null(file)) {
    log <- read_lines(file)
  }
}

#' Parse general combat events: damage, heal, power, absorb, supercharge.
#'
#' @param log Character vector of log lines.
#' @param file Name of a log file to read from.
#' @param time_digits Number of decimal places to use for time (seconds).
#' @return A combat_events object.
parse_combat <- function(log, file=NULL, time_digits=3) {
  
  pattern <- paste0(
    "^(?<time>[0-9]{16}) ",
    "\\[(?<type>Damage|Healing|Power|Supercharge|Combat) (?>In|Out)\\] ",
    "(?<source>[^']+)'s? ",
    "(?<ability>.+?) ",
    "(?<crit>critically )?",
    "(?:(?:damag|heal)ed (?<target>.+) for|absorbed) ",
    "(?<value>[0-9]+)",
    "(?: Power| Supercharge)?$"
  )
  
  events <- parse_log(log, pattern, file=file, time_digits=time_digits, class="combat_events")

  if (nrow(events) == 0) {
    return(events)
  }
  
  events[type == "Combat", type := "absorb"]
  events[, type := tolower(type)]
  events[, crit := crit != ""]
  setcolorder(events, c("time", "source", "target", "ability", "value", "type", "crit"))
  
  # Fix names that were capitalized at the beginning of the line
  fix_cap_names(events)
  
  # Fix incorrect attribution of abilities with aura or chain effects
  fix_attribution(events)
  
  events[]
}

#' Parse combat summary events.
#'
#' @param log Character vector of log lines.
#' @param file Name of a log file to read from.
#' @param time_digits Number of decimal places to use for time (seconds)
#' @return A summary_events object.
parse_summary <- function(log, file=NULL, time_digits=3) {
  
  pattern <- paste0(
    "^(?<time>[0-9]{1,16}) ",  # time=0 bug still exists as of 2/20/17
    "\\[Summary\\] (?<type>Damage|Healing|Power) ",
    "\\[(?<interval>[0-9]+\\.[0-9]+)s\\] ",
    "(?<xps>[0-9]+)/s - ",
    "(?<total>[0-9]+) total - ",
    "(?<hits>[0-9]+) hits? \\((?<max>[0-9]+) max\\) - ",
    "(?<crits>[0-9]+) \\([0-9]+\\.[0-9]+%\\) crits? - ",
    "(?<targets>[0-9]+) targets?$"
  )

  events <- parse_log(log, pattern, file=file, time_digits=time_digits, class="summary_events")
  
  if (nrow(events) == 0) {
    return(events)
  }

  events[, type := tolower(type)]
  events[, crit_pct := round(ifelse(hits > 0, crits/hits, 0), 3)]
  setcolorder(events, c("time", "type", "interval", "xps", "total", "hits", "max", 
                        "crits", "crit_pct", "targets"))
  
  events[]
}

#' Parse crowd control effects, including counters.
#' 
#' Effects can be one of the following:
#'   knockdown, juggle, stun, pull, push, ground, encasement, snare, root, 
#'   knockback, suppression, counter
#'
#' @param log Character vector of log lines.
#' @param file Name of a log file to read from.
#' @param time_digits Number of decimal places to use for time (seconds).
#' @return A crowd_control_events object.
parse_crowd_control <- function(log, file=NULL, time_digits=3) {

  pattern <- paste0(
    "^(?<time>[0-9]{16}) ",
    "\\[Combat (?:In|Out)\\] ",
    "(?<source>.+)'s? ",
    "(?<ability>.+) ",
    "(?<effect>stunned|knocked down|juggled|pulled toward|pushed|grounded|",
    "encased|snared|rooted|knocked back|suppressed) ",
    "(?<target>.+)$"
  )
  
  events <- parse_log(log, pattern, file=file, time_digits=time_digits, class="crowd_control_events")
  
  if (nrow(events) == 0) {
    return(events)
  }
  
  effects <- c("knocked down"  = "knockdown", 
               "juggled"       = "juggle",
               "stunned"       = "stun",
               "pulled toward" = "pull",
               "pushed"        = "push",
               "grounded"      = "grounding",
               "encased"       = "encasement",
               "snared"        = "snare",
               "rooted"        = "root",
               "knocked back"  = "knockback",
               "suppressed"    = "suppression")
  
  events[, effect := effects[effect]]
  
  # Only 3 Quantum abilities inflict the actual "suppression" effect - the rest are counters 
  suppression_abilities <- c("Time Shift", "Time Bomb", "Temporal Vortex", "Warped Reality")
  events[effect == "suppression" & !ability %in% suppression_abilities, effect := "counter"]
  
  # Fix names that were capitalized at the beginning of the line
  fix_cap_names(events)
  
  events[]
}

#' Parse knockouts.
#'
#' @param log Character vector of log lines.
#' @param file Name of a log file to read from.
#' @param time_digits Number of decimal places to use for time (seconds).
#' @return A knockout_events object.
parse_knockout <- function(log, file=NULL, time_digits=3) {

  pattern <- "^(?<time>[0-9]{16}) \\[Damage (?:In|Out)\\] (?<source>.+) knocked out (?<target>.+)$"
  events <- parse_log(log, pattern, file=file, time_digits=time_digits, class="knockout_events")
  
  if (nrow(events) == 0) {
    return(events)
  }
  
  # Restoration Barrels don't count as real knockouts
  events <- events[target != "Restoration Barrel"]

  # Names are properly formatted in knockouts

  events[]
}

#' Parse dodges.
#'
#' @param log Character vector of log lines.
#' @param file Name of a log file to read from.
#' @param time_digits Number of decimal places to use for time (seconds).
#' @return A dodge_events object.
parse_dodge <- function(log, file=NULL, time_digits=3) {

  pattern <- "^(?<time>[0-9]{16}) \\[Damage (?:In|Out)\\] (?<source>.+) dodged (?<target>.+)'s? (?<ability>.+)$"
  events <- parse_log(log, pattern, file=file, time_digits=time_digits, class="dodge_events")
  
  if (nrow(events) == 0) {
    return(events)
  }
  
  # Fix names that were capitalized at the beginning of the line
  fix_cap_names(events)
  
  events[]
}

#' Parse a combat log given a pattern.
parse_log <- function(log, pattern, file=NULL, time_digits=3, class="events") {
  
  if (!is.null(file)) {
    log <- read_lines(file)
  }
  
  if (!is.character(log)) {
    stop("'log' must be a character vector")
  }
  
  if (!grepl("?<time>", pattern)) {
    stop("pattern must contain a 'time' group")
  }
  
  matches <- str_match_named(log, pattern)
  events <- as.data.table(matches)
  setattr(events, "matched", attr(matches, "matched"))
  setattr(events, "class", c(class, class(events)))
  
  if (nrow(events) == 0) {
    return(events)
  }
  
  numeric_cols <- get_numeric_groups(pattern)
  if (length(numeric_cols) > 0) {
    events[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols=numeric_cols]
  }
  
  events[, time := as.POSIXct(round(as.numeric(time)/1e6, time_digits), origin="1970-01-01")]
  
  events
}

#' Fix incorrect attribution of abilities with aura or chain reaction effects.
fix_attribution <- function(combat_events) {
  
  # Reattribute Sparring Target damage if there's only one other damage source in the log.
  sources <- combat_events[type == "damage", unique(source)]
  if (length(sources) == 2 && "Sparring Target" %in% sources) {
    true_source <- sources[sources != "Sparring Target"]
    combat_events[type == "damage" & source == "Sparring Target", source := true_source]
    return(combat_events[])
  }
  
  # Aura/chain abilities in each powerset
  aura_abilities <- list(
    electricity = c("Electrogenesis", "Voltaic Bolt", "Arc Lightning", "Overcharge"),
    nature      = c("Harvest"),
    fire        = c("Spontaneous Combustion", "Overheat", "Inferno"),
    gadgets     = c("Sticky Bomb"),
    celestial   = c("Corrupted Retribution", "Curse", "Cleansed Curse"),
    munitions   = c("Chain Grenade Launcher")
  )
  
  for (powerset in names(aura_abilities)) {
    if (any(aura_abilities[[powerset]] %in% combat_events$ability)) {
      # Most aura attacks show up in the combat log attributed to both the source and the target.
      # Sticky Bomb is probably the only exception here.
      # Eliminate any targets that appear to be damaging themselves.
      aura_dmg_events <- combat_events[type == "damage" & ability %in% aura_abilities[[powerset]]]
      sources <- aura_dmg_events[, unique(source)]
      targets <- aura_dmg_events[, unique(target)]
      
      # If there's only one remaining source, it's likely to be the true source.
      true_source <- sources[!sources %in% targets]
      if (length(true_source) == 1) {
        combat_events[type == "damage" & ability %in% aura_abilities[[powerset]], source := true_source]
      }
    }
  }
  
  combat_events[]
}

#' Fix capitalization of source names.
fix_cap_names <- function(events) {
  # Check the 'target' field for the original capitalization
  targets <- unique(events$target)
  lowercase_names <- targets[substr(targets, 0, 1) %in% base::letters]
  cap_names <- paste0(toupper(substr(lowercase_names, 0, 1)), substring(lowercase_names, 2))
  lowercase_names <- setNames(lowercase_names, cap_names)

  events[source %in% names(lowercase_names), source := lowercase_names[source]]

  events[]
}
