% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_lead_lag.R
\name{graph_lead_lag}
\alias{graph_lead_lag}
\title{Function provides plots of a time series X(t) versus its lead X(t+k) or lag X(t-k) series for multiple values of k.}
\usage{
graph_lead_lag(
  df = NULL,
  time_col = NULL,
  value_col = NULL,
  k_vector = c(1, 2, 3, 4),
  direction = "lag",
  n_columns = 2,
  title = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  rot_y_tic_label = FALSE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_fit = FALSE,
  fit_color = "blue",
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_alpha = 1,
  pts_size = 1,
  col_width = 12,
  row_height = 8,
  display_plot = TRUE,
  png_file_path = NULL
)
}
\arguments{
\item{df}{A time series data frame with columns for a time and their corresponding observed values.}

\item{time_col}{Names the column from \code{df} for the time values. Values can
be numeric or Date/POSIXct.}

\item{value_col}{Names the value column from \code{df}.}

\item{k_vector}{A vector of integers giving multiple leads or lags k to derive from the \code{value_col} series.}

\item{direction}{A string that controls either finding the next ("lead") or previous ("lag")
values within \code{values}. The default is "lag".}

\item{n_columns}{An integer that sets the number of columns in the multi-panel.}

\item{title}{A string that defines an overall title to the pair of plots.}

\item{x_limits}{A numeric 2 element vector that sets the minimum and maximum for the x axis time series values.
Use NA to refer to the existing minimum and maximum.}

\item{x_major_breaks}{A numeric vector or function that defines the exact major tic locations along the x axis.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and maximum for the y axis lag values.
Use NA to refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that defines the exact major tic locations for the y axis'.}

\item{rot_y_tic_label}{A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.}

\item{show_major_grids}{A logical that controls the appearance of major grids.}

\item{show_minor_grids}{A logical that controls the appearance of minor grids.}

\item{show_fit}{A logical that controls the appearance of a fitted line through the points.}

\item{fit_color}{A string that sets the fitted line color if \code{show_fit} is TRUE.}

\item{axis_text_size}{A numeric that sets the font size along the axis'. Default is 11.}

\item{pts_color}{A string that sets the color of the points.}

\item{pts_fill}{A string that sets the fill color of the points.}

\item{pts_shape}{A numeric integer that sets the shape of the points. Typical values are 21 "circle",
22 "square", 23 "diamond", 24 "up triangle", 25 "down triangle".}

\item{pts_stroke}{A numeric that sets the drawing width for a point shape.}

\item{pts_alpha}{A numeric value that sets the alpha level of \code{pts_color}.}

\item{pts_size}{A numeric value that sets the size of the points.}

\item{col_width}{An integer that sets the width of each plot column in centimeters.}

\item{row_height}{An integer that sets the height of each plot column in centimeters.}

\item{display_plot}{A logical that if TRUE displays the plot.}

\item{png_file_path}{A character string with the directory and file name to produce
a png image of the plot.}
}
\value{
Returning a named list with:
\enumerate{
\item "models" -- A list of the fitted linear models for each lead or lag values
submitted via \code{k_vector}.
\item "predictions" -- A list of data frames corresponding to each model with columns for
for time "DateTime", predictions "Predictions", and residuals "Residuals".
\item "plots" -- A multi-paneled TableGrob object plotting the lead or lag series with the original series.
Use \code{grid::grid.draw(plots)} to display the plots.
}
}
\description{
The multipanelled plots have the values of a time series along the x axis and its corresponding
lead/lag values along the y axis. The purpose of the plots is to pin point the values for k (i.e. the amount of
lead or lag) that show a relationship with the current observed time series X(t).
}
\details{
The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkb} package to draw
the multi-paneled plot.

If \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
the function returns a named list that includes a plot object which can be displayed from the console by entering:
\enumerate{
\item \code{grid::grid.newpage()}
\item \code{grid::grid.draw(plot object)}
}
}
\author{
Rick Dean
}
