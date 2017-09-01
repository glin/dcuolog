# TODO overall_pct not relevant when by_ability=FALSE or length source = 1
# TODO targets, dps/target, change by, change targets showing up if target=NULL, don't show time (do it somewhere else, or in attributes)


#' Summarize general combat events.
#'
#' @param events A combat_events object.
#' @param type One of "damage", "healing", "power", "absorb", "supercharge",
#'   "damage_in", "healing_in", "power_in" (damage by default).
#' @param by_ability Aggregate by ability.
#' @param power_metrics Include power in metrics (only for damage, healing, power, or absorb).
#' @param combat_window Maximum window of combat time (10 seconds by default).
#' @return A combat_summary object.
summary.combat_events <- function(events,
                                  type=c("damage", "healing", "power", "absorb", "supercharge",
                                         "damage_in", "healing_in", "power_in"),
                                  by_ability=FALSE,
                                  power_metrics=FALSE,
                                  combat_window=10) {
  
  summ_type <- match.arg(type)

  if (power_metrics && !summ_type %in% c("damage", "healing", "power", "absorb")) {
    stop("power in metrics only apply to types: damage, healing, power, or absorb")
  }
  
  if (endsWith(summ_type, "_in")) {
    summ_type <- unlist(strsplit(summ_type, "_"))[1]
    source_col <- "target"
  } else {
    source_col <- "source"
  }
  
  group_by <- c(source_col, if (by_ability) "ability")
  
  summ_combat <- events[type == summ_type,
                        list(n = .N,
                             min = suppressWarnings(min(value)),
                             max = suppressWarnings(max(value)),
                             avg = round(sum(value)/.N, 1),
                             total = sum(value),
                             crit_pct = round(sum(crit)/.N, 3)),
                        by=group_by]
  
  summ_combat[, overall_pct := round(total/sum(total), 3)]
  
  source_times <- events[, list(time = round(total_time(time), 1),
                                combat_time = round(active_time(time, window=combat_window), 1)),
                         by=source_col]
  
  summ_combat <- merge(summ_combat, source_times, by=source_col)
  summ_combat[, xps := ifelse(time > 0, round(total/time, 1), NA)]
  summ_combat[, e_xps := ifelse(time > 0, round(total/combat_time, 1), NA)]
  
  xps <- paste0(substr(summ_type, 0, 1), "ps")
  e_xps <- paste0("e_", xps)
  setnames(summ_combat, c(source_col, "xps", "e_xps"), c("source", xps, e_xps))
  
  if (power_metrics) {
    power_in <- events[type == "power", list(power_in = sum(value)), by=list(source=target)]
    summ_combat <- merge(summ_combat, power_in, by="source", all.x=TRUE)
    summ_combat[, xpp := round(ifelse(power_in > 0, total/power_in, NA), 1)]
    setnames(summ_combat, "xpp", paste0(substr(summ_type, 0, 1), "pp"))
  }

  setattr(summ_combat, "class", c("combat_summary", class(summ_combat)))
  
  summ_combat[]
}

#' Summarize combat parser summary events.
#'
#' @param events A parser_summary object.
#' @param type One of "damage", "healing", "power" (damage by default).
#' @return Summary table of combat summary events.
summary.parser_summary <- function(events, type=c("damage", "healing", "power")) {
  
  summ_type <- match.arg(type)
  summ_parser <- events[type == summ_type,
                        list(n = .N,
                             time = sum(interval),
                             xps = round(sum(total) / sum(interval), 1),
                             min_xps = suppressWarnings(min(xps)),
                             max_xps = suppressWarnings(max(xps)),
                             total = sum(total),
                             hits = sum(hits),
                             max = suppressWarnings(max(max)),
                             crits = sum(crits),
                             crit_pct = round(sum(crits) / sum(hits), 3),
                             avg_targets = round(weighted.mean(targets, interval), 1))]
  
  xps_cols <- c("xps", "min_xps", "max_xps")
  new_xps_cols <- gsub("xps", paste0(substr(summ_type, 0, 1), "ps"), xps_cols)
  setnames(summ_parser, xps_cols, new_xps_cols)
  
  setattr(summ_parser, "class", c("parser_summary", class(summ_parser)[-1]))
  
  summ_parser[]
}

#' Summarize crowd control events.
#'
#' @param events A crowd_control_events object.
#' @param by Group by any combination of "source", "target", "ability", "effect" (all by default).
#' @return Summary table of crowd control events.
summary.crowd_control_events <- function(events, by=c("source", "target", "ability", "effect")) {
  
  by <- match.arg(by, several.ok=TRUE)
  summ_crowd_control <- events[, list(n = .N), by=by]
  
  setattr(summ_crowd_control, "class", c("crowd_control_summary", class(summ_crowd_control)[-1]))
  
  summ_crowd_control[]
}

#' Summarize knockout events.
#'
#' @param events A knockout_events object.
#' @param by Group by any combination of "source", "target" (all by default).
#' @return Summary table of knockout events.
summary.knockout_events <- function(events, by=c("source", "target")) {
  
  by <- match.arg(by, several.ok=TRUE)
  summ_knockout <- events[, list(n = .N), by=by]
  
  setattr(summ_knockout, "class", c("knockout_summary", class(summ_knockout)[-1]))

  summ_knockout[]
}

#' Summarize dodge events.
#'
#' @param events A dodge_events object.
#' @param by Group by any combination of "source", "target", "ability" (all by default).
#' @return Summary table of dodge events.
summary.dodge_events <- function(events, by=c("source", "target", "ability")) {
  
  by <- match.arg(by, several.ok=TRUE)
  summ_dodge <- events[, list(n = .N), by=by]
  
  setattr(summ_dodge, "class", c("dodge_summary", class(summ_dodge)[-1]))
  
  summ_dodge[]
}

#' Finds the total elapsed time for a vector of date-time objects.
total_time <- function(time, units="secs") {
  if (length(time) <= 1) {
    return(NA)
  }
  
  result <- as.numeric(suppressWarnings(difftime(max(time), min(time), units=units)))
}

#' Finds active time within a specified window.
active_time <- function(time, window=10) {
  if (length(time) <= 1) {
    return(NA)
  }
  stopifnot(window > 0)
  
  diff_time <- diff(sort(time))
  
  repeat {
    active_intervals <- diff_time[diff_time <= window]
    result <- as.numeric(sum(active_intervals))
    if (length(active_intervals) > 0) {
      break
    }
    # double window size until we get a result
    window <- window * 2
  }
  
  result
}
