# Title     : read_tingo_key()
# Objective : Read from .Renviron the tiingo auth key for quantmod::
# Created by: Rick Dean
# Created on: 2020-01-29 1:47 PM

#' Read the tiingo authorization key for quantmod package
#'
#' @description Function reads the \code{RIINGO_TOKEN} token from the .Renviron file.
#'
#'  A .Renviron file can be created by running \code{usethis::edit_r_environ()}.  A token
#'  named RIINGO_TOKEN was defined in this file with the user's tiingo auth key gotten from the tiingo web site.
#'
#' @return returns the api key string
#'
#' @author Rick Dean
#'
#' @export
read_tingo_key <- function(){
  tiingo_key <- Sys.getenv("RIINGO_TOKEN")
  return(tiingo_key)
}