library(riingo)
library(data.table)
library(RtsaPkg)

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

# call RtsaPkg::get_tiingo_prices() with multiple symbols and 4 years of data with periodicity = "weekly" (i.e. each Friday) .
ibm_hpq_df <- RtsaPkg::get_tiingo_stock_prices(symbols = c("IBM", "HPQ"), from = "2019-01-01", to = "2022-05-30", periodicity = "weekly")
str(ibm_hpq_df)

# call get_tiingo_stock_prices() using the defaults with multiple stock index symbols
etf_ticker_2 <- c("XLY", "XLP", "XLE",
                "XLF", "XLV", "XLI", "XLB",
                "XLK", "XLU", "XLRE",
                "SPY")
stock_index_prices_tiingo_df <- RtsaPkg::get_tiingo_stock_prices(symbols = etf_ticker_2)
str(stock_index_prices_tiingo_df)
