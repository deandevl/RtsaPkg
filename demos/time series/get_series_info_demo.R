library(tseries)
library(TSstudio)
library(zoo)
library(xts)
library(RtsaPkg)

# Information on a univariate time series(ts) object:
data("bev") # wheat prices
RtsaPkg::get_series_info(series = bev)

# Information on a multivariate time series(mts) object:
data("ice.river")
RtsaPkg::get_series_info(series = ice.river)

# Information on a zoo time object EURO_Brent
RtsaPkg::get_series_info(series = TSstudio::EURO_Brent)

# Information on a xts object Michigan_CS
RtsaPkg::get_series_info(series = TSstudio::Michigan_CS)