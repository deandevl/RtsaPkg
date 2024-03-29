% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_acf.R
\name{graph_acf}
\alias{graph_acf}
\title{Function focuses on calculating and plotting the autocorrelation of an observed time series.}
\usage{
graph_acf(
  df,
  time_col = NULL,
  value_col = NULL,
  max_lag = 30,
  line_width = 1,
  line_color = "black",
  title = NULL,
  confid_level = NULL,
  CL_color = "blue",
  caption = "Value",
  obs_x_limits = NULL,
  obs_x_major_breaks = waiver(),
  obs_x_major_date_breaks = waiver(),
  obs_x_date_labels = waiver(),
  obs_y_limits = NULL,
  obs_y_major_breaks = waiver(),
  ac_x_limits = NULL,
  ac_x_major_breaks = seq(1, max_lag, 1),
  ac_y_limits = c(-1, 1),
  ac_y_major_breaks = seq(-1, 1, 0.5),
  pac_x_limits = NULL,
  pac_x_major_breaks = seq(1, max_lag, 1),
  pac_y_limits = c(-1, 1),
  pac_y_major_breaks = seq(-1, 1, 0.5),
  layout = "ver",
  show_obs = TRUE,
  show_ac = TRUE,
  show_pc = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = FALSE,
  bold_y = NULL,
  col_width = 18,
  row_height = 4,
  display_plot = TRUE,
  png_file_path = NULL
)
}
\arguments{
\item{df}{A data.frame containing a time series with both a value and time column.}

\item{time_col}{Names the column from \code{df} for the time values.  Values can
be numeric or Date/POSIXct and is required.}

\item{value_col}{Names the numeric column from \code{df} for time series values and is required.}

\item{max_lag}{An integer that sets the maximum lag for the autocorrelation and partial autocorrelation plots.}

\item{line_width}{A numeric that sets the line widths in all plots.}

\item{line_color}{A string that sets the line color in all plots.}

\item{title}{A string that defines an overall title to the plots.}

\item{confid_level}{A numeric that defines a confidence level which will be drawn over the autocorrelation plots. Typical value
is 1.96. If the value is \code{NULL}, confidence lines will not be drawn.}

\item{CL_color}{A string that sets the confidence line colors.}

\item{caption}{A string that sets the observed plot caption.}

\item{obs_x_limits}{A Date/POSIXct 2 element vector that sets the minimum and maximum for the observed time series along the x axis.
Use NA to refer to the existing minimum and maximum.}

\item{obs_x_major_breaks}{A Date/POSIXct vector or function that defines the exact major tic locations for the observed time series along the x axis.}

\item{obs_x_major_date_breaks}{For Date/POSIXct, a string containing the number and date unit for major breaks along the observed time series.
Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.}

\item{obs_x_date_labels}{For Date/POSIXct, a string containing the format codes for the observed time series x axis date format.
This can be a strftime format for each x axis tic date label.
Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.}

\item{obs_y_limits}{A numeric 2 element vector that sets the minimum and maximum for the observed series y axis.}

\item{obs_y_major_breaks}{A numeric vector or function that defines the exact major tic locations for the observed series y axis.}

\item{ac_x_limits}{A numeric 2 element vector that sets the minimum and maximum lags for the autocorrelation x axis.}

\item{ac_x_major_breaks}{A numeric vector or function that defines the exact major tic locations for the lags on the
autocorrelation x axis.}

\item{ac_y_limits}{A numeric 2 element vector that sets the minimum and maximum for the autocorrelation y axis.}

\item{ac_y_major_breaks}{A numeric vector or function that defines the exact major tic locations for the autocorrelation y axis'. Keep
in mind that the limits for an autocorrelation are +-1.}

\item{pac_x_limits}{A numeric 2 element vector that sets the minimum and maximum lags for the partial autocorrelation x axis.}

\item{pac_x_major_breaks}{A numeric vector or function that defines the exact major tic locations for the lags on the
partial autocorrelation x axis.}

\item{pac_y_limits}{A numeric 2 element vector that sets the minimum and maximum for the partial autocorrelation y axis.}

\item{pac_y_major_breaks}{A numeric vector or function that defines the exact major tic locations for the partial autocorrelation y axis'. Keep
in mind that the limits for an autocorrelation are +-1.}

\item{layout}{A string that sets the layout of the 3 plots horizontally "hor" or vertically "ver".}

\item{show_obs}{A logical which if FALSE hides the plot of observations.}

\item{show_ac}{A logical which if FALSE hides the plot of auto correlations.}

\item{show_pc}{A logical which if FALSE hides the plot of the partial auto correlations.}

\item{show_major_grids}{A logical that controls the appearance of major grids.}

\item{show_minor_grids}{A logical that controls the appearance of minor grids.}

\item{bold_y}{A numeric that plots a bold horizontal line at this y value.}

\item{col_width}{A numeric that sets the width of each plot in centimeters.}

\item{row_height}{A numeric that sets the height of each plot in centimeters.}

\item{display_plot}{A logical that if TRUE displays the plot.}

\item{png_file_path}{A character string with the directory and file name to produce
a png image of the plot.}
}
\value{
Returning a named list with:
\enumerate{
\item "acf_df" -- A data frame with column variables for lag ("lag"), and optionally auto coorelation ("acf")
and partial autocorrelation ("pacf").
\item "plots" -- A TableGrob object optionally showing any or all the original time series,
auto correlations, and partial correlations. Use \code{grid::grid.draw(plots)} to display the plots.
}
}
\description{
Function returns a multi-paneled optional plots of the observed time series, the
autocorrelation, and partial autocorrelation. The actual autocorrelation values are also returned
in a data.frame.
}
\details{
The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::multi_panel_grid}
package to draw the multi-paneled plot.

if \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
the function returns a named list that includes a plot object which can be displayed from the console by entering:
\enumerate{
\item \code{grid::grid.newpage()}
\item \code{grid::grid.draw(plot object)}
}
}
