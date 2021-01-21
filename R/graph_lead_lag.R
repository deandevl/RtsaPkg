#' Function provides plots of a time series X(t) versus its lead X(t+k) or lag X(t-k) series for multiple values of k.
#'
#' @description The multipanelled plots have the values of a time series along the x axis and its corresponding
#'  lead/lag values along the y axis. The purpose of the plots is to pin point the values for k (i.e. the amount of
#'  lead or lag) that show a relationship with the current observed time series X(t).
#'
#' The function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkb} package to draw
#'  the multi-paneled plot.
#'
#' @param df A time series data frame with columns for a time and their corresponding observed values.
#' @param time_col A string that names the column from \code{df} for the time values. Values can
#'  be numeric or Date/POSIXct.
#' @param value_col A string that names the value column from \code{df}.
#' @param n_vector A vector of integers giving multiple leads or lags k to derive from the \code{values} series.
#' @param direction A string that controls either finding the next (\dQuote{lead}) or previous (\dQuote{lag})
#'  values within \code{values}. The default is \dQuote{lag}.
#' @param columns An integer that sets the number of columns in the multi-panel.
#' @param title A string that defines an overall title to the pair of plots.
#' @param subtitle A string that defines an overall subtitle.
#' @param x_limits A numeric 2 element vector that sets the minimum and maximum for the x axis time series values.
#'  Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that defines the exact major tic locations along the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis lag values.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations for the y axis'.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_fit A logical that controls the appearance of a fitted line through the points.
#' @param fit_color A string that sets the fitted line color if \code{show_fit} is TRUE.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param display_plot A logical that if TRUE displays the plot.
#'
#' @importFrom data.table shift
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
#'
#' @return  Returning a named list with:
#' \enumerate{
#'  \item \dQuote{models} -- A list of the fitted linear models for each lead or lag values
#'  submitted via \code{n_vector}.
#'  \item \dQuote{predictions} -- A list of data frames corresponding to each model with columns for
#'   for time \dQuote{DateTime}, predictions \dQuote{Predictions}, and residuals \dQuote{Residuals}.
#'  \item \dQuote{plots} -- A multi-paneled TableGrob object plotting the lead or lag series with the original series.
#'   Use \code{grid::grid.draw(plots)} to display the plots.
#'}
#' @author Rick Dean
#'
#' @export
graph_lead_lag <- function(
  df = NULL,
  time_col = NULL,
  value_col = NULL,
  values = NULL,
  n_vector = c(1,2,3,4),
  direction = "lag",
  columns = 2,
  title=NULL,
  subtitle=NULL,
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
  pts_alpha = 1.0,
  pts_size = 1,
  col_width = 6,
  row_height = 2.6,
  display_plot = TRUE){
    plots <- list()
    models <- list()
    predictions <- list()

    for(i in seq_along(n_vector)){
      df_copy <- data.table::copy(df)
      if(direction == "lag"){
        lead_lag <- paste("lag_", n_vector[i], sep = "")
        x_title = paste(value_col,"(t-", n_vector[i], ")", sep = "")
        df_copy[[lead_lag]] <- data.table::shift(df_copy[[value_col]], n = n_vector[i], type = "lag")
      }else{
        lead_lag <- paste("lead_", n_vector[i],sep = "")
        x_title = paste(value_col,"(t+", n_vector[i], ")", sep = "")
        df_copy[[lead_lag]] <-  data.table::shift(df_copy[[value_col]], n = n_vector[i], type = "lead")
      }
      df_copy <- na.omit(df_copy)
      aplot <- RplotterPkg::create_scatter_plot(
        df = df_copy,
        aes_x = lead_lag,
        aes_y = value_col,
        title = paste(direction, n_vector[i],sep = " = "),
        x_title = x_title,
        y_title = paste(value_col,"(t)",sep = ""),
        rot_y_tic_label = rot_y_tic_label,
        x_limits = x_limits,
        x_major_breaks = x_major_breaks,
        y_limits = y_limits,
        y_major_breaks = y_major_breaks,
        show_major_grids = show_major_grids,
        show_minor_grids = show_minor_grids,
        axis_text_size = axis_text_size,
        pts_color = pts_color,
        pts_fill = pts_fill,
        pts_shape = pts_shape,
        pts_stroke = pts_stroke,
        pts_line_alpha = pts_alpha,
        pts_size = pts_size
      )
      fit_model <- stats::lm(as.formula(paste(value_col,lead_lag, sep = "~")),data = df_copy)
      prediction_df <- data.frame(
        DateTime = df_copy[[time_col]],
        Predictions = fit_model$fitted.values,
        Residuals = fit_model$residuals
      )
      predictions[[i]] <- prediction_df

      if(show_fit){
        aplot <- aplot +
          ggplot2::geom_abline(
            intercept = fit_model$coefficients[1],
            slope = fit_model$coefficients[2],
            linetype = "dashed",
            color = fit_color, size = 0.8, alpha = 0.7)
      }
      plots[[i]] <- aplot
      models[[paste(direction, n_vector[[i]], sep = "_")]] <- fit_model
    }

    # display the plots in a multipanel
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
        models = models,
        predictions = predictions,
        plots = multi_plot
      )
    )
}