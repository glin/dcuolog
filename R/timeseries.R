#' @export
timeseries <- function(events, ...) {
  UseMethod("timeseries")
}

#' @export
smooth <- function(ts, ...) {
  UseMethod("smooth")
}

#' Create a time series (1 second interval) from combat events.
#'
#' @param events A `combat_events` object.
#' @param type One of "damage", "healing", "power", "absorb", "supercharge",
#'   "damage_in", "healing_in", "power_in" (damage by default).
#' @param by_ability Aggregate by ability.
#' @return A `combat_ts` object.
#' @export
timeseries.combat_events <- function(events,
                                     type = c("damage", "healing", "power", "absorb", "supercharge",
                                            "damage_in", "healing_in", "power_in"),
                                     by_ability = FALSE) {

  type <- match.arg(type)

  if (endsWith(type, "_in")) {
    combat_type <- unlist(strsplit(combat_type, "_"))[1]
    source_col <- "target"
  } else {
    combat_type <- type
    source_col <- "source"
  }

  agg_events <- events[type == combat_type]

  if (nrow(agg_events) == 0) {
    stop("must have at least one combat event")
  }

  group_by <- c("time", source_col, if (by_ability) "ability")

  # convert to regular 1 second interval time series
  # faster to work with time as double and convert back to date-time later
  agg_events[, time := round(as.numeric(time))]
  ts <- agg_events[, list(value = sum(value)), by = group_by]

  # fill in gaps
  source_times <- ts[, list(min = min(time), max = max(time)), keyby = source_col]
  all_times <- ts[, list(
    time = do.call(seq, as.list(source_times[eval(get(source_col)), c(min, max)]))
  ),
  by = c(group_by[-1])]

  ts <- merge(ts, all_times, by = group_by, all = TRUE)
  set(ts, i = which(is.na(ts$value)), j = "value", value = 0)

  setnames(ts, source_col, "source")
  ts[, time := as.POSIXct(time, origin = "1970-01-01")]

  setattr(ts, "class", c("combat_ts", class(ts)))

  ts[]
}

#' Smooth combat time series data using a Gaussian filter window.
#'
#' @param ts A `combat_ts` object.
#' @return The smoothed `combat_ts` object.
#' @export
smooth.combat_ts <- function(ts) {

  smooth_gaussian <- function(x) {
    n <- length(x)
    window_size <- choose_window_size(n)

    if (n < window_size) {
      return(x)
    }

    pad_length <- floor(window_size/2) * 2
    x_padded <- c(rep(0, pad_length/2), x, rep(0, pad_length/2))
    wma <- wma_gaussian(x_padded, window_size)
    wma_unpadded <- wma[-seq_len(pad_length)]

    wma_unpadded
  }

  by_ability <- "ability" %in% colnames(ts)
  ts[, value := smooth_gaussian(value), by = c("source", if (by_ability) "ability")]
  ts[]
}

#' Plot combat time series data.
#'
#' @param ts A `combat_ts` object.
#' @importFrom ggplot2 ggplot geom_line
#' @export
plot.combat_ts <- function(ts) {

  by_ability <- "ability" %in% colnames(ts)

  if (by_ability) {
    if (length(unique(ts$source)) > 1) {
      color <- quote(paste(source, "|", ability))
    } else {
      color <- quote(ability)
    }
  } else {
    color <- quote(source)
  }

  ggplot(ts, aes(time, value)) + geom_line(mapping = aes_(color = color))
}

#' Choose a rolling window size based on number of samples.
#' @keywords internal
choose_window_size <- function(n) {
  stopifnot(n > 0)
  window_size <- ceiling(ifelse(n > 430, 0.03 * n, 3.1 * log(n) - 5.85))
  max(window_size, 2)
}

#' Weighted moving average using a Gaussian filter window.
#'
#' @importFrom TTR WMA
#' @keywords internal
wma_gaussian <- function(x, window_size, sd = 0.5) {
  stopifnot(window_size <= length(x))
  n <- seq_len(window_size) - 1
  window <- gaussian_window(window_size, sd = sd)
  WMA(x, window_size, wts = window)
}

#' @keywords internal
gaussian_window <- function(size, sd = 0.5) {
  stopifnot(size >= 2)
  n <- seq_len(size) - 1
  exp((-1/2) * ((n - (size-1)/2) / (sd * (size-1)/2))^2)
}
