library(ggplot2)
library(rlang)
library(grid)
library(gtable)
library(RplotterPkg)
library(RtsaPkg)

str(datasets::AirPassengers)
AirPassengers_df <- RtsaPkg::ts_to_df(ts_obj = datasets::AirPassengers)

AirPassengers_diff <- RtsaPkg::graph_dif(
  df = AirPassengers_df,
  time_col = "DateTime",
  value_col = "V1",
  title = "AirPassengers Series Differences at lag = 1",
  subtitle = "Monthly Passengers 1949 - 1960",
  show_obs = FALSE,
  show_pts = FALSE
)
