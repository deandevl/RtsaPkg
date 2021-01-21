#' Read the Quandl api key for the Quandl package
#'
#' @description Function reads the \code{QUANDL_KEY} token from the .Renviron file.
#'
#'  To edit the .Renviron file run \code{usethis::edit_r_environ()}.  An api key named QUANDL_KEY
#'  was defined in this file with the user's Quandl auth key gotten from the Quandl web site.
#'
#' @return returns the api key string
#'
#' @author Rick Dean
#'
#' @export
read_quandl_key <- function(){
  quandl_key <- Sys.getenv("QUANDL_KEY")
  return(quandl_key)
}