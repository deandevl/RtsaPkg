#' Function gets both time and starting data information on a \code{ts}, \code{zoo}, or
#'  \code{xts} time series object
#'
#' The function prints a short synopsis of a time series object
#'  and its starting observations.
#'
#' @param series Either a  \code{ts}, \code{zoo}, or \code{xts} time series object
#' @param n_obser An integer that sets the number of observations to display
#'
#' @author Rick Dean
#'
#' @importFrom zoo is.zoo
#' @importFrom xts is.xts
#' @importFrom xts periodicity
#'
#' @export
get_series_info <- function(series = NULL, n_obser = 5){
  if(!stats::is.ts(series) & !xts::is.xts(series) & !zoo::is.zoo(series)){
    stop("Argument must be 'ts', 'zoo', or 'xts' time series object.")
  }

  name_str <- base::deparse(base::substitute(series))
  info_lst <- list()
  info_lst$time_interval <- stats::deltat(series)
  info_lst$starting_cycles <- stats::cycle(series)[1:n_obser]
  info_lst$starting_values <-head(series, n_obser)

  if(stats::is.ts(series) & !stats::is.mts(series)){
    info_lst$name <- name_str
    info_lst$class <- "ts"
    info_lst$frequency <- stats::frequency(series)
    info_lst$start <- paste(stats::start(series), collapse = " ")
    info_lst$end <- paste(stats::end(series), collapse = " ")
    info_lst$length <- length(series)
    info_lst$number_of_var <- 1
  }else if(stats::is.ts(series) & stats::is.mts(series)){
    info_lst$name <- name_str
    info_lst$class <- "mts"
    info_lst$frequency <- stats::frequency(series)
    info_lst$start <- paste(stats::start(series), collapse = " ")
    info_lst$end <- paste(stats::end(series), collapse = " ")
    info_lst$length <- dim(series)[1]
    info_lst$number_of_var <- dim(series)[2]
  }else if(xts::is.xts(series)){
    info_lst$name <- name_str
    info_lst$class <- "xts"
    if(xts::periodicity(series)$scale != "minute"){
      info_lst$frequency <- xts::periodicity(series)$scale
    } else {
      info_lst$frequency <- paste(xts::periodicity(series)$frequency,
                                        xts::periodicity(series)$units,
                                        collapse = " ")
    }
    info_lst$start <- paste(stats::start(series), collapse = " ")
    info_lst$end <- paste(stats::end(series), collapse = " ")

    if(is.null(dim(series)) & !is.null(length(series))){
      info_lst$number_of_var <- 1
      info_lst$length <- length(series)
    } else if(dim(series)[2] == 1){
      info_lst$number_of_var <- dim(series)[2]
      info_lst$length <- dim(series)[1]
    } else if(dim(series)[2] > 1){
      info_lst$number_of_var <- dim(series)[2]
      info_lst$length <- dim(series)[1]
    }
  }else if(zoo::is.zoo(series)){
    info_lst$name <- name_str
    info_lst$class <- "zoo"
    info_lst$frequency <- xts::periodicity(as.ts(series))$scale
    info_lst$start <- paste(stats::start(series), collapse = " ")
    info_lst$end <- paste(stats::end(series), collapse = " ")
    info_lst$length <- length(series)
    if(is.null(dim(series)) & !is.null(length(series))){
      info_lst$number_of_var <- 1
      info_lst$length <- length(series)
    } else if(dim(series)[2] == 1){
      info_lst$number_of_var <- dim(series)[2]
      info_lst$length <- dim(series)[1]
    } else if(dim(series)[2] > 1){
      info_lst$number_of_var <- dim(series)[2]
      info_lst$length <- dim(series)[1]
    }
  }

  return(info_lst)
}
