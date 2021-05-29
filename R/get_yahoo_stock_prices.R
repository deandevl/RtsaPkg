#'
#' Get stock market prices via Yahoo
#'
#' @description Function is a wrapper around the quantmod package for returning a data frame or xts object of historic stock market prices.
#' Function provides access to various stock market parameters via the getSymbols() of quantmod.  Yahoo is the source for the data.
#' For further information on setting this package's arguments see \href{https://cran.r-project.org/web/packages/quantmod/}{quantmod}
#'
#' @param symbols A character vector of stock market symbols to be accessed.
#' @param from A string or Date object that sets the starting date in CCYY-MM-DD format.
#' @param to A string or Date object that sets the ending date in CCYY-MM-DD format.
#' @param periodicity A string that defines the sampling of the data. Accepted values are "daily", "weekly",
#'  "monthly".
#' @param return_class A string that sets the class of the return object. Acceptable values are "data.frame" or "xts".
#'
#' @return A data frame with a \code{date} column or xts object along with values for Open, High, Low, Close, Volume, Adjusted.
#'
#' @author Rick Dean
#'
#' @importFrom quantmod getSymbols
#' @importFrom purrr map
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#'
#' @export
get_yahoo_stock_prices <- function(symbols = "GOOG", from = "2019-01-01", to = "2019-12-31", periodicity = "daily", return_class = "data.frame") {
  if(length(symbols) > 1){
    stocks_lst <- purrr::map(symbols, get_prices_df, from = from, to = to, periodicity = periodicity, return_class = return_class)
    stock_prices <- data.table::rbindlist(stocks_lst, use.names = T)
  }else {
    stock_prices <- get_prices_df(symbol = symbols, from = from, to = to, periodicity = periodicity, return_class = return_class)
  }
  return(stock_prices)
}

get_prices_df <- function(symbol, from, to, periodicity, return_class){
  data_xts <- quantmod::getSymbols(
    Symbols = symbol,
    src = "yahoo",
    from = base::as.Date(from),
    to = base::as.Date(to),
    periodicity = periodicity,
    auto.assign = FALSE,
    return.class = "xts"
  )
  if(return_class == "data.frame") {
    dt <- data.table::as.data.table(data_xts)
    data.table::setnames(dt, c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"))
    dt[, `:=`(Date = base::as.Date(Date), Symbol = as.factor(symbol))]
    return(dt)
  }else{
    return(data_xts)
  }
}
