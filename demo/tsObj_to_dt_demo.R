library(data.table)
library(tsbox)
library(RtsaPkg)

# Show datasets::AirPassengers info
airpass_info_ls <- RtsaPkg::get_series_info(datasets::AirPassengers)
# Convert the datasets::AirPassengers 'ts' time series to a data.table
airpass_dt <- RtsaPkg::tsObj_to_dt(datasets::AirPassengers)

# Show datasets::EuStockMarkets info
eustockmarket_info <- RtsaPkg::get_series_info(datasets::EuStockMarkets)
# Convert the datasets::EuStockMarkets 'mts' multiple time series to a data.table
eustockmarket_dt <- RtsaPkg::tsObj_to_dt(datasets::EuStockMarkets)
