library(tseries)
library(zoo)
library(RtsaPkg)

data("bev")

# Convert a univariate time series (ts) object to a data.frame and data.table
# Note: the returned column name for the data column is "V1".
str(bev)
class(bev)

wheat_price_df <- RtsaPkg::ts_to_df(ts_obj = bev)
str(wheat_price_df)

# Repeat the above but set the returned data column name
wheat_price_named_df <- RtsaPkg::ts_to_df(ts_obj = bev, col_name = "PriceIdx")
str(wheat_price_named_df)

#Convert a multivariate time series (mts) object to a data.frame and data.table
data("ice.river")
str(ice.river)
class(ice.river)

ice_river_df <- RtsaPkg::ts_to_df(ice.river)
str(ice_river_df)