metrics <- function(events, ...) {
  UseMethod("metrics")
}


#' Finds the total elapsed time for a vector of date-time objects.
total_time <- function(time, units="secs") {
  if (length(time) <= 1) {
    return(NA)
  }
  
  as.numeric(suppressWarnings(difftime(max(time), min(time), units=units)))
}

#' Finds active time within a specified window.
active_time <- function(time, window=10) {
  if (length(time) <= 1) {
    return(NA)
  }
  stopifnot(window > 0)
  
  diff_time <- diff(sort(time))

  repeat {
    result <- as.numeric(sum(diff_time[diff_time <= window]))
    if (result > 0) {
      break
    }
    # double window until we get a result
    window <- window * 2
  }
  
  result
}
