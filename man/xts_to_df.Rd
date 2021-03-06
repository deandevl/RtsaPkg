% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xts_to_df.R
\name{xts_to_df}
\alias{xts_to_df}
\title{Convert an "xts" or "zoo" time series object to a data.frame}
\usage{
xts_to_df(xts_obj = NULL, col_names = NULL, time_class = "Date")
}
\arguments{
\item{xts_obj}{Either a univariate or multivariate time series object of class
"xts or "zoo".}

\item{col_names}{A string vector from which to set the returned data.frame's column names
including the time column.}

\item{time_class}{A string that sets the class of the returned data.frame's time column.
Acceptable values are "Date", "POSIXct, or "POSIXlt".}
}
\value{
A data.frame with a column of Date/Time and the series' data column(s).
}
\description{
Function returns a data.frame with a Date/Time
column and associated data.
}
\author{
Rick Dean
}
