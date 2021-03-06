#' Function focuses on calculating and plotting the autocorrelation of an observed time series.
#'
#'
#' @description Function returns a multi-paneled optional plots of the observed time series, the
#'  autocorrelation, and partial autocorrelation. The actual autocorrelation values are also returned
#'  in a data.frame.
#'
#' The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkb} package to draw
#'  the multi-paneled plot.
#'
#' @param df A data.frame containing a time series with both a value and time column.
#' @param time_col A string that names the column from \code{df} for the time values.  Values can
#'  be numeric or Date/POSIXct.
#' @param value_col A string that names the numeric column from \code{df} for time series values.
#' @param max_lag An integer that sets the maximum lag for the autocorrelation and partial autocorrelation plots.
#' @param line_size A numeric that sets the line widths in the autocorrelation and partial autocorrelation plots.
#' @param title A string that defines an overall title to the pair of plots.
#' @param subtitle A string that defines an overall subtitle.
#' @param x_title A string that sets the observed plot x axis title.
#' @param y_title A string that sets the observed plot y axis title.
#' @param confid_level A numeric that defines a confidence level which will be drawn over the autocorrelation plots. Typical value
#'  is 1.96. If the value is \code{NULL}, confidence lines will not be drawn.
#' @param obs_x_limits A Date/POSIXct 2 element vector that sets the minimum and maximum for the observed time series along the x axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param obs_x_major_breaks A Date/POSIXct vector or function that defines the exact major tic locations for the observed time series along the x axis.
#' @param obs_x_major_date_breaks For Date/POSIXct, a string containing the number and date unit for major breaks along the observed time series.
#'  Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param obs_x_date_labels For Date/POSIXct, a string containing the format codes for the observed time series x axis date format.
#'  This can be a strftime format for each x axis tic date label.
#'  Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param obs_y_limits A numeric 2 element vector that sets the minimum and maximum for the observed series y axis.
#' @param obs_y_major_breaks A numeric vector or function that defines the exact major tic locations for the observed series y axis.
#' @param ac_x_limits A numeric 2 element vector that sets the minimum and maximum lags for the autocorrelation x axis.
#' @param ac_x_major_breaks A numeric vector or function that defines the exact major tic locations for the lags on the
#'  autocorrelation x axis.
#' @param ac_y_limits A numeric 2 element vector that sets the minimum and maximum for the autocorrelation y axis.
#' @param ac_y_major_breaks A numeric vector or function that defines the exact major tic locations for the autocorrelation y axis'. Keep
#'  in mind that the limits for an autocorrelation are +-1.
#' @param pac_x_limits A numeric 2 element vector that sets the minimum and maximum lags for the partial autocorrelation x axis.
#' @param pac_x_major_breaks A numeric vector or function that defines the exact major tic locations for the lags on the
#'  partial autocorrelation x axis.
#' @param pac_y_limits A numeric 2 element vector that sets the minimum and maximum for the partial autocorrelation y axis.
#' @param pac_y_major_breaks A numeric vector or function that defines the exact major tic locations for the partial autocorrelation y axis'. Keep
#'  in mind that the limits for an autocorrelation are +-1.
#' @param layout A string that sets the layout of the 3 plots horizontally \dQuote{hor} or vertically \dQuote{ver}.
#' @param show_obs A logical which if FALSE hides the plot of observations.
#' @param show_ac A logical which if FALSE hides the plot of auto correlations.
#' @param show_pc A logical which if FALSE hides the plot of the partial auto correlations.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param bold_y A numeric that plots a bold horizontal line at this y value.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param display_plot A logical that if TRUE displays the plot.
#'
#' @return  Returning a named list with:
#' \enumerate{
#'  \item \dQuote{acf_df} -- A data frame with column variables for lag (\dQuote{lag}), and optionally auto coorelation (\dQuote{acf})
#'   and partial autocorrelation (\dQuote{pacf}.
#'  \item \dQuote{plots} -- A TableGrob object optionally showing any or all the original time series,
#'   auto correlations, and partial correlations. Use \code{grid::grid.draw(plots)} to display the plots.
#' }
#' @importFrom data.table data.table
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom rlang sym
#' @importFrom RplotterPkg create_stick_plot
#' @importFrom RplotterPkg multi_panel_grid
#' @import ggplot2
#'
#' @export
graph_acf <- function(
  df,
  time_col,
  value_col,
  max_lag = 10,
  line_size =0.8,
  title = NULL,
  subtitle = NULL,
  confid_level = NULL,
  x_title = "DateTime",
  y_title = "Value",
  obs_x_limits = NULL,
  obs_x_major_breaks = waiver(),
  obs_x_major_date_breaks = waiver(),
  obs_x_date_labels = waiver(),
  obs_y_limits = NULL,
  obs_y_major_breaks = waiver(),

  ac_x_limits = NULL,
  ac_x_major_breaks = seq(1,max_lag,1),
  ac_y_limits = c(-1,1),
  ac_y_major_breaks = seq(-1,1,0.2),

  pac_x_limits = NULL,
  pac_x_major_breaks = seq(1,max_lag,1),
  pac_y_limits = c(-1,1),
  pac_y_major_breaks = seq(-1,1,0.2),

  layout = "ver",
  show_obs = TRUE,
  show_ac = TRUE,
  show_pc = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  bold_y = NULL,
  col_width = 8,
  row_height = 2.6,
  display_plot = TRUE){

    df_copy <- data.table::copy(df)

    acf_df <- data.table(
      lag = seq(1,max_lag, 1)
    )
    plots <- list()

    if(show_obs){
      # create a line plot of the observed series
      obsv_plot <- RplotterPkg::create_stick_plot(
        df = df_copy,
        aes_x = time_col,
        aes_y = value_col,
        line_size = line_size,
        rot_y_tic_label = TRUE,
        title = "Observations",
        x_title = x_title,
        y_title = y_title,
        x_limits = obs_x_limits,
        x_major_breaks = obs_x_major_breaks,
        x_major_date_breaks = obs_x_major_date_breaks,
        x_date_labels = obs_x_date_labels,
        y_limits = obs_y_limits,
        y_major_breaks = obs_y_major_breaks,
        show_major_grids = show_major_grids,
        show_minor_grids = show_minor_grids,
      )
      plots$obsv <- obsv_plot
    }

    if(show_ac){
      # get autocorrelations
      acf_vals <- stats::acf(df_copy[[value_col]], plot = F, lag.max = max_lag)$acf
      acf_df[, acf := acf_vals[2:(max_lag + 1)]]

      # create stick plot for acf
      acf_plot <- RplotterPkg::create_stick_plot(
        df = acf_df,
        aes_x = "lag",
        aes_y = "acf",
        title = "ACF",
        x_title = "Lag",
        y_title = "ACF",
        rot_y_tic_label = TRUE,
        x_limits = ac_x_limits,
        x_major_breaks = ac_x_major_breaks,
        y_limits = ac_y_limits,
        y_major_breaks = ac_y_major_breaks,
        show_major_grids = show_major_grids,
        show_minor_grids = show_minor_grids,
        bold_y = bold_y
      )
      if(!is.null(confid_level)){
        acf_plot <-  acf_plot +
          geom_line(aes(y = -confid_level/sqrt(nrow(df_copy))), linetype = "dashed", color = "red", size = .8) +
          geom_line(aes(y = confid_level/sqrt(nrow(df_copy))), linetype = "dashed", color = "red", size = .8)
      }
      plots$acf <- acf_plot
    }

    if(show_pc){
      # create the partial autocorrelation data.frame
      pacf_vals <- stats::pacf(df_copy[[value_col]], plot = F, lag.max = max_lag)$acf
      acf_df[, pacf := as.numeric(pacf_vals)]

      #create stick plot for pacf
      pacf_plot <- RplotterPkg::create_stick_plot(
        df = acf_df,
        aes_x = "lag",
        aes_y = "pacf",
        title = "PACF",
        x_title = "Lag",
        y_title = "PACF",
        rot_y_tic_label = TRUE,
        x_limits = pac_x_limits,
        x_major_breaks = pac_x_major_breaks,
        y_limits = pac_y_limits,
        y_major_breaks = pac_y_major_breaks,
        show_major_grids = show_major_grids,
        show_minor_grids = show_minor_grids,
        bold_y = bold_y
      )
      if(!is.null(confid_level)){
        pacf_plot <-  pacf_plot +
          geom_line(aes(y = -confid_level/sqrt(nrow(df_copy))), linetype = "dashed", color = "red", size = .8) +
          geom_line(aes(y = confid_level/sqrt(nrow(df_copy))), linetype = "dashed", color = "red", size = .8)
      }
      plots$pacf <- pacf_plot
    }

    # display the plots in a multipanel
    columns <-  1
    if(layout == "hor"){
      columns <- length(plots)
    }

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

    return(
      list(
        acf_df = acf_df,
        plots = multi_plot
      )
    )
}