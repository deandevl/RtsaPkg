
#' Get stock market dividends via Tiingo.
#'
#' @description A wrapper around the riingo::riingo_prices()) for returning a data frame of historic stock market dividends.
#' For further information on setting this package's arguments see \href{https://cran.r-project.org/web/packages/riingo/riingo.pdf}{riingo}.
#' Note that you need to set a Tinngo API key before calling this function. See the above reference for further
#' information. See \code{RtsaPkg::read_tingo_key()} if you have saved the key at the .Renviron file.
#'
#' @param symbols A string of stock market symbols to be accessed.
#' @param periodicity A string that defines the frequency of sampled prices. Acceptable values are \dQuote{daily},
#'   \dQuote{monthly}, \dQuote{quarterly}, \dQuote{yearly}.
#' @param from A string or Date object that sets the starting date in CCYY-MM-DD format.
#' @param to A string or Date object that sets the ending date in CCYY-MM-DD format.
#'
#' @return A data frame with \dQuote{Date} and \dQuote{DivCash} along with other stock price related variables.
#'
#' @author Rick Dean
#'
#' @importFrom riingo supported_tickers
#' @importFrom riingo riingo_prices
#' @importFrom data.table as.data.table
#'
#' @export
get_tiingo_dividends <- function(symbols = "IBM", periodicity = "monthly", from = "2019-01-01", to = "2019-12-31"){
  supported_tickers_v <- riingo::supported_tickers()$ticker
  supported_v <- intersect(supported_tickers_v, symbols)
  if(length(supported_v) > 0){
    dividends_df <- riingo::riingo_prices(ticker = supported_v, start_date = from, end_date = to, resample_frequency = periodicity)
    dividends_dt <- data.table::as.data.table(dividends_df)
    data.table::setnames(dividends_dt, c("Symbol", "Date", "Close", "High", "Low", "Open",
                                      "Volume", "AdjClose", "AdjHigh", "AdjLow",
                                      "AdjOpen", "AdjVolume", "DivCash", "SplitFactor"))
    dividends_dt[, `:=`(Symbol = as.factor(Symbol), Date = base::as.Date(Date))][order(Symbol)]
    return(dividends_dt)
  }else {
    stop("Symbols supported by Tiingo were not found.")
  }
}
