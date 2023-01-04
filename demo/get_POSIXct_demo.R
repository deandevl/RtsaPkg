library(zoo)
library(astsa)
library(here)
library(lubridate)
library(data.table)
library(RplotterPkg)
library(RtsaPkg)

# Simple single time conversion.
my_time_1 <- RtsaPkg::get_POSIXct("2020-03-12 22:03:28")
class(my_time_1)

# -------------------------------------------------
# Simple single time conversion that does not follow
#   ISO 8601 format. Setting the time_format parameter.
my_time_2 <- RtsaPkg::get_POSIXct("Monday, January 21, 2017 11:59:59 PM",
                                time_format = "%A, %B %d, %Y %I:%M:%S %p")
class(my_time_2)
# -------------------------------------------------
# Looking at astsa::speech (a ts object) with a time index of a .1 second
# Convert index to POSIXct of astsa::speech
speech_ts_info_lst <- RtsaPkg::get_series_info(astsa::speech)
class(astsa::speech)

# get numeric time index vector and convert to actual study's time units of tenth of second
speech_index_v <- zoo::index(astsa::speech)/10

# create a POSIXct time object from speech_index with current time origin
speech_time_posct <- RtsaPkg::get_POSIXct(time_vals = speech_index_v, time_origin = Sys.Date())
class(speech_time_posct)

# get the corresponding data
speech_data_v <- RtsaPkg::get_series_data(astsa::speech)

# create a data.frame and plot the data
speech_df <- data.frame(
  time = speech_time_posct,
  data = speech_data_v
)
RplotterPkg::create_scatter_plot(
  df = speech_df,
  aes_x = "time",
  aes_y = "data",
  title = "Recorded Speech",
  subtitle = "source: 'astsa' package",
  x_title = "Time(min:sec)",
  y_title = "Speech Value",
  connect = T,
  x_major_date_breaks = "4 sec",
  x_date_labels = "%M:%S"
)

# Convert to POSIXct for a data set that contains a mixture of time
# The date column alternates between Date and hourly Date-Time
# Data set is Texas demand for electricity

data_file_path <- file.path(here::here(), "demo", "data", "texas-electric-demand.csv")
texas_electric_dt <- data.table::fread(data_file_path)
head(texas_electric_dt)

# The following was not successful--time has a mixture of formats
texas_electric_times <- RtsaPkg::get_POSIXct(time_vals = texas_electric_dt$date)

# The following was successful
texas_electric_times_alt <- lubridate::parse_date_time(texas_electric_dt$date, orders = c("Ymd", "Ymd HM"))

# ----------------------------
# Read in IBM stock market data
ibm_data_path <- file.path(here::here(),"demo/data/trade_ibm.csv")
ibm_data_dt <- data.table::fread(
  file = ibm_data_path,
  col.names = c("TIME","PRICE","G127","CORR","COND","EX","SIZE")
)
# Convert "TIME" column to POSIXct
ibm_time <- RtsaPkg::get_POSIXct(
  time_vals = ibm_data_dt$TIME,
  time_format = "%I:%M:%S",
  time_origin = Sys.Date()
)