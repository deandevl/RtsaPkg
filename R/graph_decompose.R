
#' Function plots an observed time series along with its decomposed seasonal, trend, and irregular components
#'
#'
#' Function uses \code{decompose} from the stats package to plot the series' observed values, seasonal,
#' trend, and irregular components using moving averages. User can set the seasonal component as additive or
#' multiplicative.
#'
#' Function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::multi_panel_grid} package to
#' draw the multi-paneled plot.
#'
#' if \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
#' the function returns a named list that includes a plot object which can be displayed from the console by entering:
#' \enumerate{
#'   \item \code{grid::grid.newpage()}
#'   \item \code{grid::grid.draw(plot object)}
#' }
#'
#' @param series_ts A time series (class ts) object.
#' @param type_comp A string that describes the type of seasonal component. Accepted values are
#'  "additive" and "multiplicative".
#' @param title A string that defines an overall title to the multi-paneled plot.
#' @param x_title A string that defines the x axis title.
#' @param x_limits A Date/POSIXct 2 element vector that sets the minimum and maximum for the x axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks A Date/POSIXct vector or function that defines the exact major tic locations along the x axis.
#' @param x_major_date_breaks For Date/POSIXct, a string containing the number and date unit for major breaks.
#'  Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels For Date/POSIXct, a string containing the format codes for the x axis date format.
#'  This can be a strftime format for each x axis tic date label.
#'  Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param col_width An integer that sets the width of each plot column in centimeters.
#' @param row_height An integer that sets the height of each plot column in centimeters.
#' @param display_plot A logical that if TRUE displays the plot.
#'
#' @return Returns a named list with:
#' \enumerate{
#'   \item "decompose_dt" -- A data.frame and data.table with column values for the
#'    observed series, time, seasonal, trend, and random.
#'   \item "plots" -- A multi-paneled TableGrob object containing line plots of the
#'    series' observed values, trend, seasonal, and random values across time.
#'    Use \code{grid::grid.draw(plots)} to display the plots.
#' }
#'
#' @author Rick Dean
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table melt
#' @importFrom rlang sym
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom zoo coredata
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom RplotterPkg multi_panel_grid
#' @importFrom purrr map
#' @import ggplot2
#'
#' @export
graph_decompose <- function(
  series_ts = NULL,
  type_comp = "additive",
  title = NULL,
  x_title = "DateTime",
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  col_width = 20.0,
  row_height = 5.0,
  display_plot = TRUE){
    series_decomp <- stats::decompose(series_ts, type = type_comp)

    decompose_dt <- data.table(
      DateTime = stats::time(series_ts),
      Observed = series_decomp$x[seq_along(series_decomp$x)],
      Seasonal = series_decomp$seasonal[seq_along(series_decomp$seasonal)],
      Trend = series_decomp$trend[seq_along(series_decomp$trend)],
      Random = series_decomp$random[seq_along(series_decomp$random)]
    )

    # convert decompose_dt to a "long" format
    decompose_long_dt <- data.table::melt(
      decompose_dt,
      id.vars = "DateTime",
      measure.vars = c("Observed","Seasonal","Trend","Random"),
      variable.name = "Source",
      value.name = "Value")

    measures <- c("Observed","Seasonal","Trend","Random")

    build_plot <- function(
      id,
      dt,
      measures,
      x_title,
      x_limits,
      x_major_breaks,
      x_major_date_breaks,
      x_date_labels)
      {
        if(id == 4){
          x_title  <-  x_title
        }else{
          x_title <- NULL
        }

        plot_dt <- dt[Source == measures[[id]], ]

        aplot <- RplotterPkg::create_scatter_plot(
          df = plot_dt,
          aes_x = "DateTime",
          aes_y = "Value",
          subtitle = measures[[id]],
          x_title = x_title,
          x_limits = x_limits,
          x_major_breaks = x_major_breaks,
          x_major_date_breaks = x_major_date_breaks,
          x_date_labels = x_date_labels,
          show_pts = F,
          show_major_grids = T,
          show_minor_grids = F,
          rot_y_tic_label = T,
          connect = T,
          silent_NA_warning = T
        )
        return(aplot)
      }

    plot_lst <- purrr::map(
      1:4,
      build_plot,
      dt = decompose_long_dt,
      measures = measures,
      x_title,
      x_limits = x_limits,
      x_major_breaks,
      x_major_date_breaks,
      x_date_labels
    )
    names(plot_lst) <- measures

    layout <- list(
      plots = plot_lst,
      rows = c(1, 2, 3, 4),
      cols = c(1, 1, 1, 1)
    )

    multi_plot <- RplotterPkg::multi_panel_grid(
      layout = layout,
      col_widths = col_width,
      row_heights = c(row_height, row_height, row_height, row_height),
      title = title
    )

    if(display_plot){
      grid::grid.newpage()
      grid::grid.draw(multi_plot)
    }else{
      return(list(
        decompose_dt = decompose_dt,
        plots = multi_plot
      ))
    }
}
