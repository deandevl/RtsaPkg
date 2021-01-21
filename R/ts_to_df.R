#'
#' Convert a "ts" time series object to a "data.frame" object
#'
#' @description  Function returns a data.frame/data.table with a Date/Time column named \dQuote{DateTime} and column(s) of
#'  associated data.
#'
#' @param ts_obj Either a univariate time series object of class \dQuote{ts} or a multivariate time series object \dQuote{mts}.
#' @param col_name If \code{ts_obj} is of class \dQuote{ts} then this argument sets the returned data column name.
#'  The default data column name is \dQuote{V1}.
#'
#' @return A data.frame/data.table with a column of time named (\dQuote{DateTime}) and the series' data column(s).
#'
#' @author Rick Dean
#'
#' @importFrom data.table as.data.table
#' @importFrom zoo coredata
#'
#' @export
ts_to_df <- function(ts_obj = NULL, col_name = NULL){
  if(!is.ts(ts_obj)){
    stop("The submitted object is not of class 'ts'")
  }
  scale_vec <- function(time_vec_float, old, new){
    # scale value from interval (min/max) 'old' to 'new'
    a_scale <- (new[2] - new[1]) / (old[2] - old[1])
    new_time_vec <- (time_vec_float - old[1]) * a_scale + new[1]
    return(new_time_vec)
  }

  days_in_month <- function(year_vec, month_vec_integer){
    # day: transform based on specific month and year (leap year?)
    date_1_vec <- as.Date(paste(year_vec, month_vec_integer, 1, sep = "-"))
    date_2_vec <- as.Date(paste(year_vec, month_vec_integer + 1, 1, sep = "-"))
    days_vec <- difftime(date_2_vec, date_1_vec)
    return(as.numeric(days_vec))
  }

  get_date <- function(year_vec, month_vec_float, month_vec_integer){
    max_days_vec <- days_in_month(year_vec, month_vec_integer)
    day_vec_float <- sapply(seq_along(year_vec), function(idx){
      scale_vec(month_vec_float[idx] - month_vec_integer[idx], c(0,1), c(1, max_days_vec[idx]))
    })
    day_vec_integer <- as.integer(day_vec_float)
    date_vec_rep <- paste(as.character(year_vec), as.character(month_vec_integer), as.character(day_vec_integer), sep = "-")
    date_vec_return <- as.Date(date_vec_rep, format = "%Y-%m-%d")
    return(date_vec_return)
  }

  float_to_date <- function(time_vec){
    # convert a float 'val' (e.g. 1999.03) to its Date representation
    year_vec <- as.integer(time_vec)
    # obtaining the month: consider decimals
    time_vec_float <- time_vec - year_vec
    # months: transform from [0,1) value range to [1,12] value range
    month_vec_float <- scale_vec(time_vec_float, c(0,1), c(1,12))
    month_vec_integer <- as.integer(month_vec_float)
    date_vec_return <- get_date(year_vec, month_vec_float, month_vec_integer)
    return(date_vec_return)
  }

  dt <- data.table::as.data.table(zoo::coredata(ts_obj))
  if("ts" %in% class(ts_obj) & !is.null(col_name) ){
    colnames(dt) = col_name
  }
  date_vec <- float_to_date(as.numeric(time(ts_obj)))
  dt[, DateTime := date_vec]
  return(dt)
}

