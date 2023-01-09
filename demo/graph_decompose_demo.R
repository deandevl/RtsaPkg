library(ggplot2)
library(rlang)
library(data.table)
library(zoo)
library(grid)
library(gtable)
library(RplotterPkg)
library(RtsaPkg)

# plot the observed AirPassengers data set
RtsaPkg::graph_decompose(
  series_ts = datasets::AirPassengers,
  type_comp = "multiplicative",
  title = "AirPassengers Data Set",
  x_title = "Year"
)
