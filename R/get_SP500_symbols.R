#' Get stock market symbols for companies in the S&P 500 Index
#'
#' @description Returns a character vector of S&P 500 symbols using Wikipedia as a source.
#'
#' @return A data frame with variables for S&P 500 symbols, company names, and company sectors.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom data.table data.table
#'
#' @author Rick Dean
#'
#' @export
get_SP500_symbols <- function(){
  sp500_wiki <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

  symbols_table_lst <- sp500_wiki %>%
    rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
    rvest::html_table()
  symbols_dt <-  data.table::data.table(
    Symbol = symbols_table_lst[[1]]$Symbol,
    Security = symbols_table_lst[[1]]$Security,
    GICS_Sector = symbols_table_lst[[1]]$`GICS Sector`
  )
  return(symbols_dt)
}
