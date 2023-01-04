#' Function converts a time series object to a `data.table`
#'
#' The function `tsbox::ts_df()` is incorporated to return a `data.table` from
#'   a time object of classes `ts`, `xts`, `mts`, or `zoo`. In the `data.table` the series' time index
#'   is named "time" and corresponding values as "value". If the series is a multiple time series (mts)
#'   then an index column is created (named "id") with the names of the variables.
#'
#' @param series A `ts`, `xts`, `mts`, or `zoo` time series object
#' @param wide_form A logical which if TRUE will reshape the data.table to wide form.
#'
#' @return A `data.table`
#'
#' @importFrom tsbox ts_df
#' @importFrom data.table setDT
#' @importFrom data.table dcast
#'
#' @export
tsObj_to_dt <- function(
  series = NULL,
  wide_form = FALSE
){
  dt <- data.table::setDT(tsbox::ts_df(series))
  if(wide_form){
    dt_wide <- data.table::dcast(
      dt,
      time ~ id,
      value.var = "value"
    )
    return(dt_wide)
  }
  return(dt)
}