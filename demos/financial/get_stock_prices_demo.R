
library(quantmod)
library(riingo)
library(data.table)
library(purrr)
library(xts)
library(RtsaPkg)

# --------------------------------access stock prices via Yahoo---------------------------

# call get_yahoo_stock_prices() with all the argument defaults to get a data.frame of GOOG stock prices
goog_prices_df <- RtsaPkg::get_yahoo_stock_prices()
str(goog_prices_df)

# call get_yahoo_stock_prices() with periodicity = "monthly" to get a data.frame of GOOG stock prices
goog_prices_monthly_df <- RtsaPkg::get_yahoo_stock_prices(periodicity = "monthly")
str(goog_prices_monthly_df)

# call get_yahoo_stock_prices() and get a xts object of GOOG stock prices
goog_prices_xts <- RtsaPkg::get_yahoo_stock_prices(return_class = "xts")
str(goog_prices_xts)

# call get_yahoo_stock_prices() for an unsupported symbol
unsupported_prices <- RtsaPkg::get_yahoo_stock_prices(symbols = "y223")

# call get_yahoo_stock_prices() using the defaults with multiple symbols and get a data.frame of stock index prices
# note: symbol "XLY" is not included--Yahoo problem?
etf_ticker <- c("XLP", "XLE",
                "XLF", "XLV", "XLI", "XLB",
                "XLK", "XLU", "XLRE",
                "SPY")
stock_index_prices_df <- RtsaPkg::get_yahoo_stock_prices(symbols = etf_ticker)
str(stock_index_prices_df)

# ----------------------------access stock prices via Tiingo-----------------------------------------

# call read_set_tingo_key() to read the api key for Tiingo
api_key <- RtsaPkg::read_tingo_key()
# set the api key
riingo::riingo_set_token(api_key)

# call get_tiingo_stock_prices() with all the argument defaults to get a data.frame of IBM stock prices
ibm_prices_df <- RtsaPkg::get_tiingo_stock_prices()
str(ibm_prices_df)

# call get_tiingo_stock_prices() submitting an unsupported symbol
unsupported_prices_df <- RtsaPkg::get_tiingo_stock_prices(symbols = "y223")

# call get_tiingo_stock_prices() and get the nasdaq index plus test an unsupported symbol
nasdaq_prices_df <- RtsaPkg::get_tiingo_stock_prices(symbols = c("QQQ", "y223"))
str(nasdaq_prices_df)

# call get_tiingo_stock_prices() using the defaults with multiple stock index symbols
etf_ticker_2 <- c("XLY", "XLP", "XLE",
                "XLF", "XLV", "XLI", "XLB",
                "XLK", "XLU", "XLRE",
                "SPY")
stock_index_prices_tiingo_df <- RtsaPkg::get_tiingo_stock_prices(symbols = etf_ticker_2)
str(stock_index_prices_tiingo_df)
