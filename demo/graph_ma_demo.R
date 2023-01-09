library(data.table)
library(rlang)
library(grid)
library(gtable)
library(astsa)
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
# set "display_plot" to FALSE and display from the console

birth_dt <- RtsaPkg::tsObj_to_dt(astsa::birth)
str(birth_dt)
birth_ma_lst <- RtsaPkg::graph_ma(
  df = birth_dt,
  time_col = "time",
  value_col = "value",
  window_n = 15,
  title = "Moving Average of US Births",
  y_title = "Number",
  show_pts = FALSE,
  overlap = FALSE,
  display_plot = FALSE
)
# show the moving average dataframe
str(birth_ma_lst$ma_df)
# plot the moving average from this R console
grid::grid.newpage()
grid::grid.draw(birth_ma_lst$plots)

# use ma_type = "spe" using the Spencer 15-point weighted
# show just the moving average plot
RtsaPkg::graph_ma(
  df = birth_dt,
  time_col = "time",
  value_col = "value",
  ma_type = "spe",
  overlap = FALSE,
  title = "Spencer Moving Average of US Births",
  y_title = "Number",
  x_major_breaks = seq(as.Date("1948-01-01"), as.Date("1979-01-01"), "5 year"),
  x_date_labels = "%Y",
  show_pts = FALSE,
  show_observe = FALSE
)

