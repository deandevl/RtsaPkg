#' Function creates a POSIXct time object(s) for representing calendar
#' dates and times
#'
#' The function is just a simple wrapper around the
#'  \code{base::as.POSIXct()} as a reminder of the arguments required
#'  to implement the base function.
#'
#' @param time_vals A vector/object as character in date-time format or
#'  a numeric in seconds.
#' @param time_format A character string that defines the format of
#'   \code{time_vals} if different from the ISO 8601 format. The format
#'   string should follow the format string for \code{base::strptime()}.
#' @param time_origin A character string that sets the origin/start date
#'  for calculating the internal elapsed seconds representation of the time.
#'  The default in R is January 1, 1970.
#'
#' @return A POSIXct time object
#'
#' @author Rick Dean
#'
#' @export
get_POSIXct <- function(time_vals = NULL, time_format = NULL, time_origin = NULL){
  try_formats <- c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S","%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
                   "%Y-%m-%d", "%Y:%m:%d")
  if(is.null(time_origin)){
    time_origin <- "1970-01-01"
  }
  if(!is.null(time_format)){
    times <- as.POSIXct(x = time_vals, format = time_format, origin = time_origin)
  }else{
    times <- as.POSIXct(x = time_vals, tryFormats = try_formats, origin = time_origin)
  }
  return(times)
}
