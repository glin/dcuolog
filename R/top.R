#' @export
top <- function(events, ...) {
  UseMethod("top")
}

#' Show top events in the combat log.
#'
#' @param events A `combat_events` object.
#' @param type One of "damage", "healing", "power", "absorb", "supercharge" (damage by default).
#' @param n Number of events (30 by default).
#' @return The top n events in the log.
#' @importFrom utils head
#' @export
top.combat_events <- function(events,
                              n = 30,
                              type = c("damage", "healing", "power", "absorb", "supercharge")) {

  stopifnot(n >= 0)

  event_type <- match.arg(type)
  top_events <- events[type == event_type]
  sort_dt(top_events, by = "value", order = "desc")

  head(top_events, n)
}
