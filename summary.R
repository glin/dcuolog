#' Summarize general combat events.
#'
#' @param events A combat_events object.
#' @param type One of "damage", "healing", "power", "absorb", "supercharge" (damage by default).
#' @param by Group by any combination of "source", "target", "ability" (all by default).
#' @param source Character vector of sources to filter by (optional).
#' @param target Character vector of targets to filter by (optional).
#' @param ability Character vector of abilities to filter by (optional).
#' @param crit Filter by crit=TRUE or crit=FALSE (optional).
#' @param sort_by Column to sort results by (optional).
#' @param sort_order Sort in "desc" or "asc" order (desc by default).
#' @return A combat_summary object.
summary.combat_events <- function(events,
                                  type=c("damage", "healing", "power", "absorb", "supercharge"),
                                  by=c("source", "target", "ability"),
                                  source=NULL,
                                  target=NULL,
                                  ability=NULL,
                                  crit=NULL,
                                  sort_by=NULL,
                                  sort_order=c("desc", "asc")) {
  
  type <- match.arg(type)
  by <- match.arg(by, several.ok=TRUE)
  
  select_expr <- quote(list(n = .N,
                            min = suppressWarnings(min(value)),
                            max = suppressWarnings(max(value)),
                            avg = round(sum(value)/.N, 1),
                            total = sum(value),
                            crit_pct = round(sum(crit)/.N, 5)))
  
  summ_events <- summarize_events(events, select_expr, by,
                                  type=type,
                                  source=source,
                                  target=target,
                                  ability=ability,
                                  crit=crit,
                                  class="combat_summary")
  
  summ_events[, overall_pct := round(total/sum(total), 5)]
  
  if (!is.null(sort_by)) {
    sort_dt(summ_events, sort_by, sort_order)
  }

  summ_events[]
}

#' Summarize crowd control events.
#'
#' @param events A crowd_control_events object.
#' @param by Group by any combination of "source", "target", "ability", "effect" (all by default).
#' @param source Character vector of sources to filter by (optional).
#' @param target Character vector of targets to filter by (optional).
#' @param ability Character vector of abilities to filter by (optional).
#' @param effect Character vector of effects to filter by (optional).
#' @param sort_by Column to sort results by (optional).
#' @param sort_order Sort in "desc" or "asc" order (desc by default).
#' @return A crowd_control_summary object.
summary.crowd_control_events <- function(events,
                                         by=c("source", "target", "ability", "effect"),
                                         source=NULL,
                                         target=NULL,
                                         ability=NULL,
                                         effect=NULL,
                                         sort_by=NULL,
                                         sort_order=c("desc", "asc")) {
  
  group_by <- match.arg(by, several.ok=TRUE)
  
  select_expr = quote(list(n = .N))
  
  summ_events <- summarize_events(events, select_expr, by,
                                  source=source,
                                  target=target,
                                  ability=ability,
                                  effect=effect,
                                  class="crowd_control_summary")
  
  if (!is.null(sort_by)) {
    sort_dt(summ_events, sort_by, sort_order)
  }
  
  summ_events[]
}

#' Summarize knockout events.
#'
#' @param events A knockout_events object.
#' @param by Group by any combination of "source", "target" (all by default).
#' @param source Character vector of sources to filter by (optional).
#' @param target Character vector of targets to filter by (optional).
#' @param sort_by Column to sort results by (optional).
#' @param sort_order Sort in "desc" or "asc" order (desc by default).
#' @return A knockout_summary object.
summary.knockout_events <- function(events,
                                    by=c("source", "target"),
                                    source=NULL,
                                    target=NULL,
                                    sort_by=NULL,
                                    sort_order=c("desc", "asc")) {
  
  group_by <- match.arg(by, several.ok=TRUE)
  
  select_expr = quote(list(n = .N))
  
  summ_events <- summarize_events(events, select_expr, by,
                                  source=source,
                                  target=target,
                                  class="knockout_summary")
  
  if (!is.null(sort_by)) {
    sort_dt(summ_events, sort_by, sort_order)
  }
  
  summ_events[]
}

#' Summarize dodge events.
#'
#' @param events A dodge_events object.
#' @param by Group by any combination of "source", "target", "ability" (all by default).
#' @param source Character vector of sources to filter by (optional).
#' @param target Character vector of targets to filter by (optional).
#' @param ability Character vector of abilities to filter by (optional).
#' @param sort_by Column to sort results by (optional).
#' @param sort_order Sort in "desc" or "asc" order (desc by default).
#' @return A dodge_summary object.
summary.dodge_events <- function(events,
                                 by=c("source", "target", "ability"),
                                 source=NULL,
                                 target=NULL,
                                 ability=NULL,
                                 sort_by=NULL,
                                 sort_order=c("desc", "asc")) {
  
  group_by <- match.arg(by, several.ok=TRUE)
  
  select_expr = quote(list(n = .N))
  
  summ_events <- summarize_events(events, select_expr, by,
                                  source=source,
                                  target=target,
                                  ability=ability,
                                  class="dodge_summary")
  
  if (!is.null(sort_by)) {
    sort_dt(summ_events, sort_by, sort_order)
  }
  
  summ_events[]
}

#' Summarize events.
#'
#' @param events An events object.
#' @param select_expr Expression for selecting columns.
#' @param by Columns to group by.
#' @param ... Additional arguments can be a vector of values used to filter the specified column.
#' @param class Class name of the summary object.
summarize_events <- function(events,
                             select_expr,
                             by,
                             ...,
                             class="summary") {
  
  by <- match.arg(by, choices=names(events), several.ok=TRUE)
  
  where <- list(...)
  where <- where[names(where) %in% names(events) & !sapply(where, is.null)]
  
  where_expr <- TRUE
  for (name in names(where)) {
    expr <- bquote(.(as.symbol(name)) %in% where[[.(name)]])
    where_expr <- paste(c(where_expr, expr), collapse="&")
  }
  
  summ_events <- events[eval(parse(text=where_expr)),
                        eval(select_expr),
                        by=by]
  
  setattr(summ_events, "class", c(class, class(summ_events)[-1]))
  setattr(summ_events, "summary_info", c(list(by = by), where))
  
  summ_events[]
}

#' Get info about a summary object.
summary_info <- function(x) {
  attr(x, "summary_info")
}
