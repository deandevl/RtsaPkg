library(data.table)
library(riingo)
library(RtsaPkg)

# call read_set_tingo_key() to read the api key for tiingo
api_key <- RtsaPkg::read_tingo_key()
# set the Tiingo token
riingo::riingo_set_token(api_key)

# call RtsaPkg::get_tiingo_dividends() with all the argument defaults
ibm_div_df <- RtsaPkg::get_tiingo_dividends()
str(ibm_div_df)

# call RtsaPkg::get_tiingo_dividends() and attempt to get dividends from an unsupported symbol.
unsupported_df <- RtsaPkg::get_tiingo_dividends(symbols = "y223")

# call RtsaPkg::get_tiingo_dividends() with multiple symbols and 4 years of data. Also add a none supported symbol.
div_df <- RtsaPkg::get_tiingo_dividends(symbols = c("IBM", "HPQ", "y223"), from = "2015-01-01", to = "2019-12-31")
str(div_df)

# get dividends for a group of nasdaq companies
nasdaq_ticker <- c("IBM", "HPQ", "TXN", "CSCO", "INTC", "ORCL", "AAPL", "MSFT", "QCOM")
nasdaq_div_df <- RtsaPkg::get_tiingo_dividends(symbols = nasdaq_ticker)
str(nasdaq_div_df)

