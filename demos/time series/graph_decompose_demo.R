library(ggplot2)
library(rlang)
library(data.table)
library(zoo)
library(grid)
library(gtable)
library(TSstudio)
library(RplotterPkg)
library(RtsaPkg)

# USVSales from TSstudio
decompose_USVSales <- RtsaPkg::graph_decompose(
  series_ts = TSstudio::USVSales,
  title = "Monthly US Vehicle Sales 1976-2020",
  x_title = "Year",
  y_title = "Sales",
  show_minor_grids = FALSE,
  silent_NA_warning = TRUE
)

# plot the observed AirPassengers data set
decompose_AirPassengers <- RtsaPkg::graph_decompose(
  series_ts = datasets::AirPassengers,
  type_comp = "multiplicative",
  title = "AirPassengers Data Set",
  x_title = "Year",
  y_title = "Passengers",
  show_minor_grids = FALSE,
  silent_NA_warning = TRUE
)
