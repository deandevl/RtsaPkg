#' Function converts a data.frame to an xts time object
#'
#' The data.frame must have a time based Date or POSIXct column along
#'  with one or more data columns. Function returns an \code{xts} time object.
#'
#' @param df A data.frame containing time and data columns
#' @param time_col A string that is the name for the time column from \code{df}.
#' @param data_cols A string or vector string containing the name(s) of data columns from \code{df}.
#'
#' @return An \code{xts} time series object
#'
#' @author Rick Dean
#'
#' @importFrom data.table setDT
#' @importFrom xts xts
#'
#' @export
df_to_xts <- function(df, time_col, data_cols){
  dt <- data.table::setDT(df)
  return_xts <- xts::xts(dt[, ..data_cols], order.by = dt[,get(time_col)])
  return(return_xts)
}