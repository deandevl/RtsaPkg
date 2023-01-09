library(rlang)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)
library(tsbox)
library(RplotterPkg)
library(RtsaPkg)

# simulate a series that has a significant autoregression at lags 1 and 2
# set the layout as horizontal and show only the acf and pacf autocorrelations
ar_2 <- arima.sim(list(ar = c(0.65, 0.3)), n = 1000)
ar_2_df <- RtsaPkg::tsObj_to_dt(ar_2)
RtsaPkg::graph_acf(
  df = ar_2_df,
  time_col = "time",
  value_col = "value",
  max_lag = 12,
  ac_y_limits = c(-0.2, 1.0),
  ac_y_major_breaks = seq(-0.2, 1.0, 0.2),
  pac_y_limits = c(-0.2, 1.0),
  pac_y_major_breaks = seq(-0.2, 1.0, 0.2),
  confid_level = 1.96,
  title = "Autocorrelations of Simulated ARIMA(2,0,0) Series",
  layout = "hor",
  col_width = 9,
  show_obs = FALSE,
  show_minor_grids = FALSE,
  bold_y = 0.0
)

