#' Retrieve the time component of a ts/zoo/xts time series object
#'
#' @description Function will retrieve the time component of a
#'  \code{ts},\code{zoo} or \code{xts} object. The retrieved time
#'  will be converted into one of several possible time classes.
#'
#' @param series Either a \code{ts},\code{zoo} or \code{xts} time series object.
#' @param time_class A string that sets the class for the converted index.
#' Acceptable values are: dQuote{Date}, dQuote{POSIXct}, dQuote{yearmon}, dQuote{yearqtr}.
#' @param time_origin A character string that sets the origin/start date
#'  for calculating the internal elapsed seconds representation of the time.
#'  The default in R is January 1, 1970.
#'
#' @return A Date/POSIXct/yearmon/yearqtr class object
#'
#' @author Rick Dean
#'
#' @importFrom zoo index
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.yearqtr
#'
#' @export
get_series_time <- function(series, time_class = "Date", time_origin = NULL){
  if(!stats::is.ts(series) & !xts::is.xts(series) & !zoo::is.zoo(series)){
    stop("Argument must be 'ts', 'zoo', or 'xts' time series object.")
  }
  if(is.null(time_origin)){
    time_origin <- "1970-01-01"
  }

  if(stats::is.ts(series)){
    ts_df <- RtsaPkg::ts_to_df(series)
    idx <- ts_df$DateTime
  }else{
    idx <- zoo::index(series)
  }

  if(time_class == "Date"){
    if(stats::is.ts(series)){
      return(idx)
    }else{
      return(zoo::as.Date(idx))
    }
  }else if(time_class == "POSIXct"){
    return(as.POSIXct(idx, origin = time_origin))
  }else if(time_class == "yearmon"){
    return(zoo::as.yearmon(idx))
  }else if(time_class == "yearqtr"){
    return(zoo::as.yearqtr(idx))
  }
}
