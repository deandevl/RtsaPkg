#' Convert an "xts" time series object to a data.frame/data.table object
#'
#' @description  Function returns a data.frame/data.table with a Date/Time
#'  column named \dQuote{index} and column(s) of associated data.
#'
#' @param xts_obj Either a univariate or multivariate time series object of class \dQuote{xts}.
#' @param time_class A string that sets the class of the returned data.frame's time column.
#'  Acceptable values are \dQuote{Date}, \dQuote{POSIXct}, or \dQuote{POSIXlt}
#' @param col_names A string vector from which to set the returned data.frame's column names.
#'  The first column is a date or time column.
#'
#' @return A data.frame/data.table with a column of time named (\dQuote{index}) and the series' data column(s).
#'
#' @author Rick Dean
#'
#' @importFrom data.table as.data.table
#' @importFrom xts convertIndex
#' @importFrom xts is.xts
#'
#' @export
xts_to_df <- function(xts_obj = NULL, col_names = NULL, time_class = "Date"){
  if(!xts::is.xts(xts_obj)){
    stop("The submitted object is not of class 'xts'.")
  }
  if(time_class == "Date"){
    xts_new <- xts::convertIndex(xts_obj, "Date")
  }else if(time_class == "POSIXct") {
    xts_new <- xts::convertIndex(xts_obj, "POSIXct")
  }else if(time_class == "POSIXlt"){
    xts_new <- xts::convertIndex(xts_obj, "POSIXlt")
  }
  dt <- data.table::as.data.table(xts_new)

  if(!is.null(col_names)){
    colnames(dt) <- col_names
  }

  return(dt)
}