#' Function provides plots of a time series X(t) versus its lead X(t+k) or lag X(t-k) series for multiple values of k.
#'
#' The multipanelled plots have the values of a time series along the x axis and its corresponding
#'  lead/lag values along the y axis. The purpose of the plots is to pin point the values for k (i.e. the amount of
#'  lead or lag) that show a relationship with the current observed time series X(t).
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
#' @param df A time series data frame with columns for a time and their corresponding observed values.
#' @param time_col Names the column from \code{df} for the time values. Values can
#'  be numeric or Date/POSIXct.
#' @param value_col Names the value column from \code{df}.
#' @param k_vector A vector of integers giving multiple leads or lags k to derive from the \code{value_col} series.
#' @param direction A string that controls either finding the next ("lead") or previous ("lag")
#'  values within \code{values}. The default is "lag".
#' @param n_columns An integer that sets the number of columns in the multi-panel.
#' @param title A string that defines an overall title to the pair of plots.
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
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 "circle",
#'  22 "square", 23 "diamond", 24 "up triangle", 25 "down triangle".
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param col_width An integer that sets the width of each plot column in centimeters.
#' @param row_height An integer that sets the height of each plot column in centimeters.
#' @param display_plot A logical that if TRUE displays the plot.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
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
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 ggsave
#'
#' @return  Returning a named list with:
#' \enumerate{
#'  \item "models" -- A list of the fitted linear models for each lead or lag values
#'  submitted via \code{k_vector}.
#'  \item "predictions" -- A list of data frames corresponding to each model with columns for
#'   for time "DateTime", predictions "Predictions", and residuals "Residuals".
#'  \item "plots" -- A multi-paneled TableGrob object plotting the lead or lag series with the original series.
#'   Use \code{grid::grid.draw(plots)} to display the plots.
#'}
#' @author Rick Dean
#'
#' @export
graph_lead_lag <- function(
  df = NULL,
  time_col = NULL,
  value_col = NULL,
  k_vector = c(1,2,3,4),
  direction = "lag",
  n_columns = 2,
  title=NULL,
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
  col_width = 12,
  row_height = 8,
  display_plot = TRUE,
  png_file_path = NULL
){

  if(is.null(time_col) | is.null(value_col)) {
    stop("Both time_col and value_col are required")
  }

  plots <- list()
  models <- list()
  predictions <- list()

  for(i in seq_along(k_vector)){
    dt <- data.table::copy(df)
    if(direction == "lag"){
      lead_lag_col_name <- paste0("lag_", k_vector[i])
      x_title <- paste0(value_col,"(t-", k_vector[i], ")")
      dt[[lead_lag_col_name]] <- data.table::shift(dt[[value_col]], n = k_vector[i], type = "lag")
    }else{
      lead_lag_col_name <- paste0("lead_", k_vector[i])
      x_title <-  paste0(value_col,"(t+", k_vector[i], ")")
      dt[[lead_lag_col_name]] <-  data.table::shift(dt[[value_col]], n = k_vector[i], type = "lead")
    }
    dt <- na.omit(dt)
    aplot <- RplotterPkg::create_scatter_plot(
      df = dt,
      aes_x = lead_lag_col_name,
      aes_y = value_col,
      title = paste(direction, k_vector[i],sep = " = "),
      x_title = x_title,
      y_title = paste0(value_col,"(t)"),
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

    fit_model <- stats::lm(as.formula(paste(value_col, lead_lag_col_name, sep = "~")),data = dt)

    prediction_df <- data.frame(
      DateTime = dt[[time_col]],
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
    models[[paste(direction, k_vector[[i]], sep = "_")]] <- fit_model
  }

    # display the plots in a multipanel
  cols <- c()
  for(i in seq_along(plots)){
    val <- i %% n_columns
    if(val == 0){
      cols <- c(cols, n_columns)
    }else {
      cols <- c(cols,val)
    }
  }
  rows <- c()
  n_rows <- ceiling(length(plots)/n_columns)
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
    return(
      list(
        models = models,
        predictions = predictions,
        plots = multi_plot
      )
    )
  }
}
