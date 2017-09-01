top <- function(events, ...) {
  UseMethod("top")
}

#' Show top events in the combat log.
#'
#' @param events A combat_events object.
#' @param type One of "damage", "healing", "power", "absorb", "supercharge" (damage by default).
#' @param n Number of events (30 by default).
#' @return The top n events in the log.
top.combat_events <- function(events,
                              type=c("damage", "healing", "power", "absorb", "supercharge"),
                              n=30) {
  
  stopifnot(n >= 0)
  
  event_type <- match.arg(type)
  top_events <- events[type == event_type]
  top_events <- top_events[, -"type"]

  setorderv(top_events, "value", order=-1L)
  
  head(top_events, n)
}
