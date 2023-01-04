#' Get stock market prices via Tiingo
#'
#' A wrapper around the riingo::riingo_prices()) for returning a data frame of historic stock prices.
#' For further information on setting this package's arguments see \href{https://cran.r-project.org/web/packages/riingo/riingo.pdf}{riingo}.
#' Note that you need to set a Tinngo API key before calling this function. See the above reference for further
#' information. See \code{RtsaPkg::read_tingo_key()} if you have saved the key at the .Renviron file.
#'
#' @param symbols A character vector of stock market symbols to be accessed.
#' @param periodicity A string that defines the frequency of sampled prices. Acceptable values are "daily",
#'   "monthly", "quarterly", "yearly".
#' @param from A string or Date object that sets the starting date in CCYY-MM-DD format.
#' @param to A string or Date object that sets the ending date in CCYY-MM-DD format.
#'
#' @return A data frame with historic "Date"'s along with stock price related variables.
#'
#' @author Rick Dean
#'
#' @importFrom riingo supported_tickers
#' @importFrom riingo riingo_prices
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#'
#' @export
get_tiingo_stock_prices <- function(symbols = "IBM", periodicity = "daily", from = "2019-01-01", to = "2019-12-31"){
  supported_tickers_v <- riingo::supported_tickers()$ticker
  supported_v <- base::intersect(supported_tickers_v, symbols)
  if(length(supported_v) > 0){
    prices_df <- riingo::riingo_prices(ticker = supported_v, start_date = from, end_date = to, resample_frequency = periodicity)
    prices_dt <- data.table::as.data.table(prices_df)
    data.table::setnames(prices_dt, c("Symbol", "Date", "Close", "High", "Low", "Open",
                                      "Volume", "AdjClose", "AdjHigh", "AdjLow",
                                      "AdjOpen", "AdjVolume", "DivCash", "SplitFactor"))
    prices_dt[, `:=`(Symbol = as.factor(Symbol), Date = base::as.Date(Date))][order(Symbol)]
    return(prices_dt)
  }else {
    stop("Symbols supported by Tiingo were not found.")
  }
}