% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_crowd_control}
\alias{parse_crowd_control}
\title{Parse crowd control effects, including counters.}
\usage{
parse_crowd_control(log, file = NULL, time_digits = 3)
}
\arguments{
\item{log}{Character vector of log lines.}

\item{file}{Name of a log file to read from.}

\item{time_digits}{Number of decimal places to use for time (seconds).}
}
\value{
A \code{crowd_control_events} object.
}
\description{
Effects can be one of the following:
knockdown, juggle, stun, pull, push, ground, encasement, snare, root,
knockback, suppression, counter
}
