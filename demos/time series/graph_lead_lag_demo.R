library(ggplot2)
library(data.table)
library(rlang)
library(grid)
library(gtable)
library(RplotterPkg)
library(RtsaPkg)

simple_series_df <- data.frame(
  DateTime = seq(as.Date("2000-01-01"), by = "1 month", length.out = 10),
  Values = c(1,2,3,4,5,6,7,8,9,10)
)
# simple lag test using default of k lags 1 to 4
series_lag <- RtsaPkg::graph_lead_lag(
  df = simple_series_df,
  time_col = "DateTime",
  value_col = "Values",
  title = "Simple Lags from 1 to 4",
  x_limits = c(1,10),
  x_major_breaks = seq(1,10,1),
  y_limits = c(1,10),
  y_major_breaks = seq(1,10,1)
)

# simple lead test using default k leads 1 to 4
series_lead <- RtsaPkg::graph_lead_lag(
  df = simple_series_df,
  time_col = "DateTime",
  value_col = "Values",
  title = "Simple Leads from 1 to 4",
  direction = "lead",
  x_limits = c(1,10),
  x_major_breaks = seq(1,10,1),
  y_limits = c(1,10),
  y_major_breaks = seq(1,10,1)
)

# simulate autoregressive processes AR(1) with coefficients 0.85
# use the defaults of the function where direction is "lag" and
# lags are n_vector = c(1,2,3,4)
ar_1_ts <- arima.sim(list(ar = 0.85), n = 300)
str(ar_1_ts)
ar_1_df <- RtsaPkg::ts_to_df(ar_1_ts)
str(ar_1_df)
ar_1 <- RtsaPkg::graph_lead_lag(
  df = ar_1_df,
  time_col = "DateTime",
  value_col = "V1",
  title = "Value(t) vs Value(t-k) for Simulated AR(1)",
  subtitle = "Coefficient = 0.85",
  show_fit = TRUE
)

summary(ar_1$models$lag_1)
str(ar_1$models$lag_1$model)

# simulate autoregressive processes AR(2) with coefficients 0.85 and -0.6858
# set the direction to "lead" and use default leads n_vector = c(1,2,3,4)
ar_2_ts <- arima.sim(list(ar = c(0.85, -0.6858)), n = 300)
str(ar_2_ts)
ar_2_df <- RtsaPkg::ts_to_df(ar_2_ts)
str(ar_2_df)
ar_2 <- RtsaPkg::graph_lead_lag(
  df = ar_2_df,
  time_col = "DateTime",
  value_col = "V1",
  direction = "lead",
  title = "Value(t) vs Value(t+k) for Simulated AR(2)",
  subtitle = "Coefficients = (0.85,-0.6858)",
  show_fit = TRUE
)

summary(ar_2$models$lead_3)
str(ar_2$models$lead_3$model)
