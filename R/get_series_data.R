#' Retrieve the observations of a ts/zoo/xts time series object
#'
#' @description Function will retrieve the data of a
#'  \code{ts},\code{zoo} or \code{xts} object.
#'
#' @param series Either a \code{ts},\code{zoo} or \code{xts} time object.
#'
#' @return A numeric/character vector of observations
#'
#' @author Rick Dean
#'
#' @importFrom  zoo coredata is.zoo
#' @importFrom xts is.xts
#'
#' @export
get_series_data <- function(series = NULL){
  if(!stats::is.ts(series) & !xts::is.xts(series) & !zoo::is.zoo(series)){
    stop("Argument must be 'ts', 'zoo', or 'xts' time series object.")
  }
  return(zoo::coredata(series))
}