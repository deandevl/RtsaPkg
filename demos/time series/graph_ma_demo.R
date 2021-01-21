
library(data.table)
library(tseries)
library(TSstudio)
library(rlang)
library(grid)
library(gtable)
library(zoo)
library(RplotterPkg)
library(RtsaPkg)


# simple number test using "sma" ma_type with window_length = 3
# show both the observed and moving average overlapped (the default)
simple_series <- data.frame(
  dates = seq(as.Date("1990-01-01"), length = 10, by= "year"),
  values = seq(1,10,1)
)
RtsaPkg::graph_ma(
  df = simple_series,
  time_col = "dates",
  value_col = "values",
  ma_type = "sma",
  window_n = 3,
  y_major_breaks = seq(1,10,1),
  title = "Simple MA"
)

# simple moving average using default "sma" ma_type with window_n = 15
# show both the observed and moving average overlapped in separate panels
data(bev)
wheat_index_df <- RtsaPkg::ts_to_df(bev)
str(wheat_index_df)
wheat_index_ma <- RtsaPkg::graph_ma(
  df = wheat_index_df,
  time_col = "DateTime",
  value_col = "V1",
  window_n = 15,
  title = "Moving Average of Beveridge Wheat Prices",
  subtitle = "1500 to 1854 (window_length = 7)",
  y_axis_title = "Prices",
  show_pts = FALSE,
  overlap = FALSE
)
str(wheat_index_ma$ma_df)

# use ma_type = "spe" using the Spencer 15-point weighted
# show just the moving average plot
wheat_index_spe <- RtsaPkg::graph_ma(
  df = wheat_index_df,
  time_col = "DateTime",
  value_col = "V1",
  ma_type = "spe",
  overlap = FALSE,
  title = "Spencer Moving Average of Beveridge Wheat Prices",
  subtitle = "1500 to 1854",
  y_axis_title = "Prices",
  x_major_breaks = seq(as.Date("1500-01-01"), as.Date("1869-01-01"), "20 year"),
  x_date_labels = "%Y",
  show_pts = FALSE,
  show_observe = FALSE
)
str(wheat_index_spe$ma_df)

# look at TSstudio::USVSales
# simple moving average using default "sma" ma_type with window_n = 12
# show both the observed and moving average overlapped (the default)
USVsales_df <- RtsaPkg::ts_to_df(ts_obj = TSstudio::USVSales)
USVsales_ma <- RtsaPkg::graph_ma(
  df = USVsales_df,
  time_col = "DateTime",
  value_col = "V1",
  window_n = 12,
  title = "US Vehicle Sales - SMA (Order = 12)",
  y_limits = c(500,1850),
  y_major_breaks = seq(500,2000, 150),
  y_axis_title = "Sales",
  show_pts = FALSE,
  palette_colors = c("black","red")
)

# look at tseries::ice.river highly seasonal temperatures
# simple moving average using default "sma" ma_type with window_n = 6
# show observed and moving average in separate panels
data("ice.river")
ice_river_df <- RtsaPkg::ts_to_df(ice.river)
str(ice_river_df)
ice_river_ma <- RtsaPkg::graph_ma(
  df = ice_river_df,
  time_col = "DateTime",
  value_col = "temp",
  window_n = 6,
  title = "Simple Moving Average of Mean Daily Tempertures in Hveravellir (deg C)",
  subtitle = "Jan 1972 to Dec 1974  Window size = 6",
  y_axis_title = "Temperature",
  show_pts = FALSE,
  overlap = FALSE
)


