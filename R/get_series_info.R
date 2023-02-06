#' Function gets both time and starting data information on a \code{ts}, \code{zoo}, or
#'  \code{xts} time series object
#'
#' The function returns a short synopsis data.frame of a time series object
#'  and optionally its starting observations.
#'
#' @param series Either a  \code{ts}, \code{zoo}, or \code{xts} time series object
#' @param n_obser An integer that sets the number of observations to display. If NULL
#'   then no observations are included in the data.frame.
#'
#' @return A named list with the series statistics(named "stats") and sample values(named "data").
#'
#' @author Rick Dean
#'
#' @importFrom data.table as.data.table
#' @importFrom zoo is.zoo
#' @importFrom xts is.xts
#' @importFrom xts periodicity
#'
#' @export
get_series_info <- function(series = NULL, n_obser = NULL){
  if(!stats::is.ts(series) & !xts::is.xts(series) & !zoo::is.zoo(series)){
    stop("Argument must be 'ts', 'zoo', or 'xts' time series object.")
  }
  if(is.null(n_obser)){
    n_obser = 13
  }
  name_str <- base::deparse(base::substitute(series))

  dt_data <- data.table::data.table()
  dt_stats <- data.table::data.table()

  dt_stats[, name := name_str]
  dt_stats[, class := class(series)[1]]
  dt_stats[, delta := stats::deltat(series)]
  dt_stats[, start := paste(stats::start(series), collapse = " ")]
  dt_stats[, end := paste(stats::end(series), collapse = " ")]

  if(stats::is.ts(series) & !stats::is.mts(series)){
    dt_stats[, freq := stats::frequency(series)]
    dt_stats[, length := length(series)]
    dt_stats[, nvar := 1]
  }else if(stats::is.ts(series) & stats::is.mts(series)){
    dt_stats[, freq := stats::frequency(series)]
    dt_stats[, length := dim(series)[1]]
    dt_stats[, nvar := dim(series)[2]]
  }else if(xts::is.xts(series)){
    if(xts::periodicity(series)$scale != "minute"){
      dt_stats[, freq := xts::periodicity(series)$scale]
    } else {
      dt_stats[,
        freq := paste(xts::periodicity(series)$frequency,
                      xts::periodicity(series)$units,
                      collapse = " ")]
    }

    if(is.null(dim(series)) & !is.null(length(series))){
      dt_stats[, length := length(series)]
      dt_stats[, nvar := 1]
    } else if(dim(series)[2] == 1){
      dt_stats[, length := dim(series)[1]]
      dt_stats[, nvar := dim(series)[2]]
    } else if(dim(series)[2] > 1){
      dt_stats[, length := dim(series)[1]]
      dt_stats[, nvar := dim(series)[2]]
    }
  }else if(zoo::is.zoo(series)){
    dt_stats[, freq := xts::periodicity(as.ts(series))$scale]
    dt_stats[, length := length(series)]

    if(is.null(dim(series)) & !is.null(length(series))){
      dt_stats[, nvar := 1]
      dt_stats[, length := length(series)]
    } else if(dim(series)[2] == 1){
      dt_stats[, nvar := dim(series)[2]]
      dt_stats[, length := dim(series)[1]]
    } else if(dim(series)[2] > 1){
      dt_stats[, nvar := dim(series)[2]]
      dt_stats[, length := dim(series)[1]]
    }
  }

  n_series <- n_obser
  if(dt_stats$length < n_obser){
    n_series <- dt_stats$length
  }

  dt_data[, cycle := stats::cycle(series)[1:n_series]]
  dt_data[, time := stats::time(series)[1:n_series]]
  m_dt <- as.data.table(head(series, n_series))

  if(dt_stats$class == "ts" | dt_stats$class == "xts"){
    data.table::setnames(m_dt, old = "V1", new = name_str, skip_absent = T)
  }else if(dt_stats$class == "zooreg"){
    data.table::setnames(m_dt, old = "x", new = name_str, skip_absent = T)
  }

  if(dt_stats$class == "xts"){
    m_dt[, index := NULL]
  }

  dt_data <- cbind(dt_data, m_dt)

  return(list(
    data = dt_data,
    stats = dt_stats
  ))
}
