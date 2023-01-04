#' Function adds a column of class `Date` to a `data.frame`
#'
#' Given a vector of the submitted `data.frame's` column names that have time related values (such as
#' year, month, day etc.), the function will add a column to the submitted `data.frame`
#' that has a class of `Date`. The function uses the package function `anytime::anydate()` to
#' compute the `Date` values.
#'
#' @param dt The target `data.frame` or `data.table` for which a column of class `Date` will be appended.
#' @param time_columns A vector of column names from `dt` that have time related numeric values. The names should
#'   be listed in the year:month:day:seconds order.
#' @param column_name A string that sets the name of the new `Date` column.
#'
#' @return A new `data.table` with an added `Date` column
#'
#' @importFrom data.table setDT
#' @importFrom data.table setnames
#' @importFrom anytime anydate
#'
#' @author Rick Dean
#'
#' @export
add_date_column <- function(dt, time_columns, column_name){
  a_dt <- data.table::setDT(dt)
  a_dt$new <- apply(a_dt[,..time_columns], 1, paste, collapse="-")
  a_dt[, new := anytime::anydate(new)]
  data.table::setnames(a_dt, old = "new", new = column_name)

  return(a_dt)
}
