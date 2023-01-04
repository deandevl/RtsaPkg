library(here)
library(data.table)
library(xts)
library(RplotterPkg)
library(RtsaPkg)

# Consider the Lake Erie data that is a series of n = 40
#   consecutive annual measurements of the level of Lake Erie
#   in October.  The source: https://online.stat.psu.edu/stat510/lesson/3/3.1.

# Read the data.
data_path <- file.path(here::here(), "demo/data/eriedata.dat")
erie_v <- base::scan(file = data_path)

# Define the data.frame.
erie_dt <- data.table::data.table(
  datetime = seq(from = as.POSIXct("1980-10-01 12:10:10"), by = "1 year", length.out = 40),
  data = erie_v
)

# Plot the series.
RplotterPkg::create_scatter_plot(
  df = erie_dt,
  aes_x = "datetime",
  aes_y = "data",
  subtitle = "Annual Lake Erie Level Measurements",
  connect = TRUE
)

# Convert the data.frame to a xts object
erie_xts <- RtsaPkg::df_to_xts(
  df = erie_dt,
  time_col = "datetime",
  data_cols = "data"
)

# Get info on the new xts object
erie_xts_info_lst <- RtsaPkg::get_series_info(series = erie_xts)
