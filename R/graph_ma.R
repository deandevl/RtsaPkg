#' Function focuses on calculating and plotting the moving average of an observed time series.
#'
#'
#' @description Function returns either an overlapped or multi-paneled plot of the observed time series,
#'  along with the plot for the moving average. The actual moving average values are also returned in a data.frame.
#'
#' The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkb} package to draw
#'  the multi-paneled plot.
#'
#' @param df A data frame with variables for times and corresponding values.
#' @param time_col A string that names the column from \code{df} for the time values. Values can
#'  be numeric or Date/POSIXct.
#' @param value_col A string that names the value column from \code{df}.
#' @param window_n An integer that controls the backward window length of the moving average.
#' @param ma_type A string that sets the type of moving average. Accepted values are \dQuote{sma} simple, \dQuote{tri} triangular,
#'  \dQuote{wma} weighted, \dQuote{exp} exponential, \dQuote{mod} modified, and \dQuote{spe} Spencer weighted 15 point average.
#' @param overlap A logical which if \code{TRUE} overlaps both the observed and the moving average series'. If \code{FALSE} the
#'  plots are in separate panels.
#' @param title A string that sets the plots overall title.
#' @param subtitle A string that sets the plots overall subtitle.
#' @param x_axis_title A string that defines the x axis title.
#' @param y_axis_title A string that defines the y axis title.
#' @param x_limits A Date/POSIXct 2 element vector that sets the minimum and maximum for the x axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks A Date/POSIXct vector or function that defines the exact major tic locations along the x axis.
#' @param x_major_date_breaks For Date/POSIXct, a string containing the number and date unit for major breaks.
#'  Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels For Date/POSIXct, a string containing the format codes for the x axis date format.
#'  This can be a strftime format for each x axis tic date label.
#'  Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations for the moving average y axis'.
#' @param show_pts A logical which if FALSE will plot only the lines.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_observe A logical that controls the appearance of the observed time series.
#' @param palette_colors A character vector to set the palette colors.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param display_plot A logical that if TRUE displays the plot.
#'
#' @return  Returning a named list with:
#' \enumerate{
#'  \item \dQuote{ma_df} -- A data.frame/data.table with column variables for time \dQuote{DateTime} and the moving average values \dQuote{Value} and
#'  source of the values \dQuote{Source}.
#'  \item \dQuote{plots} -- A multi-panelled/overlapped TableGrob object plotting the observed series and the moving averages.
#'   Use \code{grid::grid.draw(plots)} to display the plots.
#' }
#'
#' @author Rick Dean
#'
#' @importFrom data.table data.table
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
#'
#' @export
graph_ma <- function(
  df=NULL,
  time_col=NULL,
  value_col=NULL,
  window_n = 4,
  ma_type = "sma",
  overlap = TRUE,
  title=NULL,
  subtitle=NULL,
  x_axis_title = time_col,
  y_axis_title = value_col,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_pts = TRUE,
  show_observe = TRUE,
  palette_colors = NULL,
  col_width = 10,
  row_height = 3.5,
  display_plot = TRUE
){

  dates <- df[[time_col]]
  values <- df[[value_col]]

  get_simple_ma <- function(values, window_n){
    values_n <- length(values)
    simple_ma <- numeric(values_n)
    for (k in 1:(window_n-1)) {
      simple_ma[k] <- mean(values[1:k])
    }
    for (k in window_n:values_n){
      simple_ma[k] <- mean(values[(k - window_n + 1):k])
    }
    return(simple_ma)
  }

  if(ma_type == "sma"){
    ma_name <- "Simple Moving Average"
    ma = get_simple_ma(values = values, window_n = window_n)
  }else if(ma_type == "tri"){
    ma_name <- "Triangular Moving Average"
    win_n <- ceiling((window_n + 1)/2)
    ma_1 <- get_simple_ma(values = values, window_n = win_n)
    ma <- get_simple_ma(values = ma_1, window_n = win_n)
  }else if(ma_type == "wma"){
    ma_name <- "Weighted Moving Average"
    values_n <- length(values)
    ma <- numeric(values_n)

    for(k in 1:(window_n-1)) {
      divisor <- (k * (k + 1)) / 2
      ma[k] <- sum((k:1) * values[k:1]) / divisor
    }
    divisor <- (window_n * (window_n + 1)) / 2
    for(k in window_n:values_n){
      vec <- (window_n:1) * values[k:(k - window_n + 1)]
      ma[k] <- sum(vec) / divisor
    }
  }else if(ma_type == "exp"){
    ma_name <- "Exponential Moving Average"
    values_n <- length(values)
    ma <- numeric(values_n)
    wt <- 2 / (window_n + 1)
    ma[1] <- values[1]
    for(k in 2:values_n)  ma[k] <- wt * values[k] + (1 - wt) * ma[k-1]
  }else if(ma_type == "mod"){
    ma_name <- "Modified Moving Average"
    values_n <- length(values)
    ma <- numeric(values_n)
    ma[1] <- values[1]
    for(k in 2:values_n) ma[k] <- ma[k-1] + (values[k] - ma[k-1])/window_n
  }else if(ma_type == "spe"){
    ma_name <- "Spencer Moving Average"
    values_n <- length(values) - 15
    dates <- dates[1:values_n]
    ma <- numeric(values_n)
    weights <- c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3)/320
    ma <- numeric(values_n)
    for(k in 1:values_n){
      vals <- values[k:(k + 14)]
      ma[k] <- sum(vals * weights, na.rm = TRUE)
    }
  }
  ma_dt <- data.table(
    DateTime = dates,
    Value = ma
  )
  if(!show_observe){
    plots <- RplotterPkg::create_scatter_plot(
      df = ma_dt,
      aes_x = "DateTime",
      aes_y = "Value",
      title = title,
      subtitle = subtitle,
      x_title = x_axis_title,
      y_title = y_axis_title,
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
    if(display_plot){
      grid::grid.newpage()
      grid::grid.draw(plots)
    }
    return(list(
      ma_df = ma_dt,
      plots = plots
    ))
  }else if(!overlap){
    #create a line plot of the observed series
    obsv_plot <- RplotterPkg::create_scatter_plot(
      df = df,
      aes_x = time_col,
      aes_y = value_col,
      rot_y_tic_label = TRUE,
      title = "Observations",
      x_title = x_axis_title,
      y_title = y_axis_title,
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

    # create a line plot of the ma series
    ma_plot <- RplotterPkg::create_scatter_plot(
      df = ma_dt,
      aes_x = "DateTime",
      aes_y = "Value",
      rot_y_tic_label = TRUE,
      title = ma_name,
      x_title = x_axis_title,
      y_title = y_axis_title,
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

    # put both plots in a list to display them in separate panels
    plots <- list(obsv_plot,ma_plot)

    # display the plots in a multipanel
    columns <-  1
    cols <- c()
    for(i in seq(1, length(plots), by = 1)){
      val <- i %% columns
      if(val == 0){
        cols <- c(cols, columns)
      }else {
        cols <- c(cols,val)
      }
    }
    n_rows <- ceiling(length(plots)/columns)
    rows <- c()
    for(i in seq(1, n_rows, by = 1)){
      for(ii in seq(1, columns, by = 1)){
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
      col_widths = rep(col_width, columns),
      row_heights = rep(row_height, n_rows),
      title = title,
      subtitle = subtitle,
      do_legend = FALSE,
      display_plot = display_plot
    )

    return(list(
      ma_df = ma_dt,
      plots = multi_plot
    ))
  }else{
    observe_dt <- data.table(
      DateTime = df[[time_col]],
      Value = df[[value_col]],
      Source = "Observed"
    )

    ma_dt[, Source := ma_name]
    plot_df <- rbind(observe_dt, ma_dt)

    plots <- RplotterPkg::create_scatter_plot(
      df = plot_df,
      aes_x = "DateTime",
      aes_y = "Value",
      aes_color = "Source",
      title = title,
      subtitle = subtitle,
      x_title = x_axis_title,
      y_title = y_axis_title,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_major_date_breaks = x_major_date_breaks,
      x_date_labels = x_date_labels,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      show_pts = show_pts,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      palette_colors = palette_colors,
      connect = TRUE
    )
    if(display_plot){
      grid::grid.newpage()
      grid::grid.draw(plots)
    }
    return(list(
      ma_df = ma_dt,
      plots = plots
    ))
  }
}
