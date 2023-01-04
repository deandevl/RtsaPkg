#' Function creates Date time object(s) for representing calendar dates
#'
#' The function is just a simple wrapper around the
#'  \code{base::as.Date()} as a reminder of the arguments required
#'  to implement the base function. Provides a list of "try out"
#'  formats.
#'
#' @param date_vals A character string or vector of character
#'   strings of time in the assumed ISO 8601 format of "YYYY-mm-dd".
#' @param date_format A character string that defines the format of
#'   \code{date_vals} if different from the ISO 8601 format. The format
#'   string should follow the format string for \code{base::strptime()}.
#'
#' @return A Date object(s)
#'
#' @author Rick Dean
#'
#' @export
get_Date <- function(date_vals = NULL, date_format = NULL){
  try_formats <- c("%Y-%m-%d", "%Y:%m:%d", "%Y/%m/%d", "%m/%d/%Y", "%m:%d:%Y")
  if(!is.null(date_format)){
    dates <- as.Date(x = date_vals, format = date_format)
  }else {
    dates <- as.Date(x = date_vals, tryFormats = try_formats)
  }
  return(dates)
}