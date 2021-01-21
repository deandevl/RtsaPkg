library(rlang)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)
library(tseries)
library(RplotterPkg)
library(RtsaPkg)

data("ice.river")

# the "prec" series of ice.river has a significant acf at lag 1
ice_river_df <- RtsaPkg::ts_to_df(ice.river)

str(ice_river_df)

# the parf is showing slight significance at lag 1 and lag 7 (seasonal peak?)
precip_ar <- RtsaPkg::graph_acf(
  df = ice_river_df,
  time_col = "DateTime",
  value_col = "prec",
  max_lag = 12,
  title = "Time Series and ACF for Daily Precipitation in Hveravellir (mm)",
  subtitle = "Jan 1972 to Dec 1974",
  y_title = "Precipitation",
  obs_y_major_breaks = seq(0,80,10),
  ac_y_limits = c(-0.2, 0.4),
  ac_y_major_breaks = seq(-0.2, 0.4, 0.1),
  pac_y_limits = c(-0.2, 0.4),
  pac_y_major_breaks = seq(-0.2, 0.4, 0.1),
  show_minor_grids = FALSE,
  bold_y = 0.0,
  confid_level = 1.96
)

# simulate a series that has a significant autoregression at lags 1 and 2
# set the layout as horizontal and show only the acf and pacf autocorrelations
ar_2 <- arima.sim(list(ar = c(0.65, 0.3)), n = 1000)
ar_2_df <- RtsaPkg::ts_to_df(ts_obj = ar_2)
ar_2_ar <- RtsaPkg::graph_acf(
  df = ar_2_df,
  time_col = "DateTime",
  value_col = "V1",
  max_lag = 12,
  ac_y_limits = c(-0.2, 1.0),
  ac_y_major_breaks = seq(-0.2, 1.0, 0.2),
  pac_y_limits = c(-0.2, 1.0),
  pac_y_major_breaks = seq(-0.2, 1.0, 0.2),
  confid_level = 1.96,
  title = "Autocorrelations of Simulated ARIMA(2,0,0) Series",
  layout = "hor",
  show_obs = FALSE,
  show_minor_grids = FALSE,
  bold_y = 0.0,
  col_width = 6,
  row_height = 5
)

