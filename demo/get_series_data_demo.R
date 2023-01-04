library(zoo)
library(xts)
library(RtsaPkg)

# Get the data of a ts time object.
chicken_ts_info <- RtsaPkg::get_series_info(series = astsa::chicken)
chicken_ts_data_v <- RtsaPkg::get_series_data(series = astsa::chicken)
str(chicken_ts_data_v)

# Get the data from a multiple time series of measurements on blood
blood_mts_info <- RtsaPkg::get_series_info(series = astsa::blood)
blood_mts_data_mt <- RtsaPkg::get_series_data(series = astsa::blood)
head(blood_mts_data_mt)