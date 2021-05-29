#' Convert an "xts" or "zoo" time series object to a data.frame
#'
#' @description  Function returns a data.frame with a Date/Time
#'  column and associated data.
#'
#' @param xts_obj Either a univariate or multivariate time series object of class
#'  "xts or "zoo".
#' @param time_class A string that sets the class of the returned data.frame's time column.
#'  Acceptable values are "Date", "POSIXct, or "POSIXlt".
#' @param col_names A string vector from which to set the returned data.frame's column names
#'  including the time column.
#'
#' @return A data.frame with a column of Date/Time and the series' data column(s).
#'
#' @author Rick Dean
#'
#' @importFrom data.table as.data.table
#' @importFrom xts convertIndex
#' @importFrom xts is.xts
#' @importFrom zoo is.zoo
#'
#' @export
xts_to_df <- function(xts_obj = NULL, col_names = NULL, time_class = "Date"){
  if(is.zoo(xts_obj)){
    xts_obj <- as.xts(xts_obj)
  }else if(!xts::is.xts(xts_obj)){
    stop("The submitted object is not of class 'xts'.")
  }

  if(time_class == "Date"){
    xts_new <- xts::convertIndex(xts_obj, "Date")
  }else if(time_class == "POSIXct") {
    xts_new <- xts::convertIndex(xts_obj, "POSIXct")
  }else if(time_class == "POSIXlt"){
    xts_new <- xts::convertIndex(xts_obj, "POSIXlt")
  }

  df <- data.table::as.data.table(xts_new)
  if(!is.null(col_names)){
    colnames(df) <- col_names
  }

  return(df)
}
