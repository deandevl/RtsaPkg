library(ggplot2)
library(rlang)
library(grid)
library(gtable)
library(RplotterPkg)
library(RtsaPkg)

str(datasets::AirPassengers)
AirPassengers_dt <- RtsaPkg::tsObj_to_dt(datasets::AirPassengers)

# graph differences at lag 1
RtsaPkg::graph_dif(
  df = AirPassengers_dt,
  time_col = "time",
  value_col = "value",
  title = "AirPassengers Series Differences at lag = 1",
  show_obs = TRUE,
  show_pts = FALSE
)
