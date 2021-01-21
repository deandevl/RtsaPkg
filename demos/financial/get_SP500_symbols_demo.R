library(xml2)
library(rvest)
library(data.table)
library(RtsaPkg)

symbols_sp500_df <- RtsaPkg::get_SP500_symbols()
str(symbols_sp500_df)
