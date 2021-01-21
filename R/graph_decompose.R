
#' Function plots an observed time series along with its decomposed seasonal, trend, and irregular components
#'
#'
#' @description Function uses \code{decompose} from the stats package to plot the series' observed values, seasonal,
#' trend, and irregular components using moving averages. User can set the seasonal component as additive or multiplicative.
#'
#' Function uses the \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg} package to draw the multi-paneled plot.
#'
#' @param series_ts A time series (ts) object.
#' @param type_comp A string that describes the type of seasonal component. Accepted values are
#'  \dQuote{additive} and \dQuote{multiplicative}.
#' @param title A string that defines an overall title to the multi-paneled plot.
#' @param subtitle A string that defines an overall subtitle.
#' @param x_title A string that defines the x axis title.
#' @param y_title A string that defines the y axis title.
#' @param x_limits A Date/POSIXct 2 element vector that sets the minimum and maximum for the x axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks A Date/POSIXct vector or function that defines the exact major tic locations along the x axis.
#' @param x_major_date_breaks For Date/POSIXct, a string containing the number and date unit for major breaks.
#'  Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels For Date/POSIXct, a string containing the format codes for the x axis date format.
#'  This can be a strftime format for each x axis tic date label.
#'  Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#'
#' @return Returns a named list with:
#' \enumerate{
#'   \item \dQuote{decompose_df} -- A data.frame and data.table with column values for the
#'    observed series, time, seasonal, trend, and random.
#'   \item \dQuote{plots} -- A multi-paneled TableGrob object containing line plots of the
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
#' @importFrom RplotterPkg multi_scatter_plot
#' @import ggplot2
#'
#' @export
graph_decompose <- function(
  series_ts = NULL,
  type_comp = "additive",
  title = NULL,
  subtitle = NULL,
  x_title = "DateTime",
  y_title = "Value",
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  col_width = 10,
  row_height = 2.4,
  silent_NA_warning = FALSE){
    series_dt <- RtsaPkg::ts_to_df(series_ts)
    series_decomp <- stats::decompose(series_ts, type = type_comp)

    decompose_dt <- data.table(
      DateTime = series_dt$DateTime,
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

    plots <- RplotterPkg::multi_scatter_plot(
      df = decompose_long_dt,
      factor_var = "Source",
      factor_x = "DateTime",
      columns = 1,
      aes_y = "Value",
      title = title,
      subtitle = subtitle,
      x_title = x_title,
      y_titles = c(y_title,y_title,y_title,y_title),
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_major_date_breaks = x_major_date_breaks,
      x_date_labels = x_date_labels,
      show_pts = FALSE,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      col_width = col_width,
      row_height = row_height,
      rot_y_tic_label = TRUE,
      connect = TRUE,
      display_plot = TRUE,
      silent_NA_warning = silent_NA_warning
    )

    return (
      list(
        decompose_df = decompose_dt,
        plots = plots
      )
    )
}