#' Function focuses on calculating and plotting the lagged difference across a time series.
#'
#' The lagged differences across a time series at a certain degree \code{dif_lag}
#'  is taken primarily to transform the series from a non-stationary to stationary process.
#'  Function returns a multi-paneled plot of the observed time series (optional), along with a
#'  plot for the series' lagged differences. The actual difference values are also returned
#'  in a data.frame.
#'
#' The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkb} package to draw
#'  the multi-paneled plot.
#'
#' If \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
#' the function returns a named list that includes a plot object which can be displayed from the console by entering:
#' \enumerate{
#'   \item \code{grid::grid.newpage()}
#'   \item \code{grid::grid.draw(plot object)}
#' }
#'
#' @param df A data.frame containing a time series with both a value and time column.
#' @param time_col Names the column from \code{df} for the time values. Values can
#'  be numeric or Date/POSIXct.
#' @param value_col Names the numeric column from \code{df} for time series values.
#' @param dif_lag An integer that sets the lag of difference to use.
#' @param title A string that defines an overall title to the pair of plots.
#' @param subtitle A string that defines an overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param x_limits A Date/POSIXct 2 element vector that sets the minimum and maximum for the x axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks A Date/POSIXct vector or function that defines the exact major tic locations along the x axis.
#' @param x_major_date_breaks For Date/POSIXct, a string containing the number and date unit for major breaks.
#'  Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels For Date/POSIXct, a string containing the format codes for the x axis date format.
#'  This can be a strftime format for each x axis tic date label.
#'  Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the difference y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations for the difference y axis'.
#' @param show_pts A logical which if FALSE will plot only the lines.
#' @param show_obs A logical which if FALSE hides the plot of observations.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param display_plot A logical that if TRUE displays the plot.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
#'
#' @return  Returning a named list with:
#' \enumerate{
#'  \item "diff_df" -- A data frame with column variables for time "DateTime" and
#'   the difference values "Value".
#'  \item "plots" -- A multi-paneled TableGrob object plotting the differences and optionally
#'   the observed series. Use \code{grid::grid.draw(plots)} to display the plots.
#' }
#'
#' @author Rick Dean
#'
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom rlang sym
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom RplotterPkg multi_panel_grid
#' @import ggplot2
#' @importFrom ggplot2 ggsave
#'
#' @export
graph_dif <- function(
  df = NULL,
  time_col = NULL,
  value_col = NULL,
  dif_lag = 1,
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_title = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  show_pts = TRUE,
  show_obs = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  col_width = 10,
  row_height = 4,
  display_plot = TRUE,
  png_file_path = NULL
){
  if(is.null(time_col) | is.null(value_col)) {
    stop("Both time_col and value_col are required")
  }

  if(is.null(x_title)){
    x_title <- time_col
  }
  if(is.null(y_title)){
    y_title <- value_col
  }

  dates <- df[[time_col]]
  values <- df[[value_col]]
  differences <- diff(values, dif_lag)

  diff_df <- data.frame(
    datetime = dates[1:(length(dates) - dif_lag)],
    diffvalue = differences
  )
  plots <- list()

  if(show_obs){
  #create a line plot of the observed series
    obsv_plot <- RplotterPkg::create_scatter_plot(
      df = df,
      aes_x = time_col,
      aes_y = value_col,
      rot_y_tic_label = TRUE,
      title = "Observations",
      x_title = x_title,
      y_title = y_title,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_major_date_breaks = x_major_date_breaks,
      x_date_labels = x_date_labels,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      show_pts = show_pts,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      connect = TRUE
    )
    plots$obsv <- obsv_plot
  }

  # create a line plot of the diff series
  diff_plot <- RplotterPkg::create_scatter_plot(
    df = diff_df,
    aes_x = "datetime",
    aes_y = "diffvalue",
    rot_y_tic_label = TRUE,
    title = "Lagged Differences",
    x_title = x_title,
    y_title = y_title,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
    x_major_date_breaks = x_major_date_breaks,
    x_date_labels = x_date_labels,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    show_pts = show_pts,
    show_major_grids = show_major_grids,
    show_minor_grids = show_minor_grids,
    connect = TRUE
  )

  plots$diff <- diff_plot

  # display the plots in a multipanel
  n_columns <-  1
  cols <- c()
  for(i in seq(1, length(plots), by = 1)){
    val <- i %% n_columns
    if(val == 0){
      cols <- c(cols, n_columns)
    }else {
      cols <- c(cols,val)
    }
  }
  n_rows <- ceiling(length(plots)/n_columns)
  rows <- c()
  for(i in seq(1, n_rows, by = 1)){
    for(ii in seq(1, n_columns, by = 1)){
      rows <- c(rows, i)
    }
  }
  multi_layout <- list(
    plots = plots,
    rows = rows,
    cols = cols
  )

  multi_plot <- RplotterPkg::multi_panel_grid(
    layout = multi_layout,
    col_widths = rep(col_width, n_columns),
    row_heights = rep(row_height, n_rows),
    title = title,
    subtitle = subtitle,
    display_plot = FALSE
  )

  if(!is.null(png_file_path)){
    ggplot2::ggsave(
      filename = png_file_path,
      plot = multi_plot,
      device = "png",
      width = col_width * n_columns * 1700,
      height = row_height * n_rows * 1700,
      units = "px",
      scale = .05,
      dpi = 72
    )
  }

  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(multi_plot)
  }else{
    return(list(
      diff_df = diff_df,
      plots = multi_plot
    ))
  }
}
