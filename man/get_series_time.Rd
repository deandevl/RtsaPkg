% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_series_time.R
\name{get_series_time}
\alias{get_series_time}
\title{Retrieve the time component of a ts/zoo/xts time series object}
\usage{
get_series_time(series, time_class = "Date", time_origin = NULL)
}
\arguments{
\item{series}{Either a \code{ts},\code{zoo} or \code{xts} time series object.}

\item{time_class}{A string that sets the class for the converted index.
Acceptable values are: "Date", "POSIXct", "yearmon", "yearqtr".}

\item{time_origin}{A character string that sets the origin/start date
for calculating the internal elapsed seconds representation of the time.
The default in R is January 1, 1970.}
}
\value{
A Date/POSIXct/yearmon/yearqtr class object
}
\description{
Function will retrieve the time component of a
\code{ts},\code{zoo} or \code{xts} object. The retrieved time
will be converted into one of several possible time classes.
}
\author{
Rick Dean
}
