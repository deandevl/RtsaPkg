library(zoo)
library(xts)
library(TSstudio)
library(RtsaPkg)

# get the data of a ts time object
RtsaPkg::get_series_info(series = TSstudio::USgas)
USgas_data <- RtsaPkg::get_series_data(TSstudio::USgas)
str(USgas_data)

# get the data of zoo time object EURO_Brent
RtsaPkg::get_series_info(series = TSstudio::EURO_Brent)
EURO_Brent_data <- RtsaPkg::get_series_data(TSstudio::EURO_Brent)
str(EURO_Brent_data)

# get the data of Michigan_CS xts object
RtsaPkg::get_series_info(series = TSstudio::Michigan_CS)
Michigan_CS_data <- RtsaPkg::get_series_data(TSstudio::Michigan_CS)
str(Michigan_CS_data)