library(zoo)
library(TSstudio)
library(astsa)
library(RtsaPkg)
library(RplotterPkg)

# simple single time conversion
my_time <- RtsaPkg::get_POSIXct("2020-03-12 22:03:28")

# simple single time conversion that does not follow ISO 8601 format
my_time <- RtsaPkg::get_POSIXct("Monday, January 21, 2017 11:59:59 PM",
                                time_format = "%A, %B %d, %Y %I:%M:%S %p")

# Looking at astsa::speech with a time index of a .1 second
# Convert index to POSIXct of astsa::speech
TSstudio::ts_info(astsa::speech)

# get numeric time index vector and convert to actual study's time units of tenth of second
speech_index <- zoo::index(astsa::speech)/10

# create a POSIXct time object from speech_index with default origin
speech_time <- RtsaPkg::get_POSIXct(time_vals = speech_index, time_origin = Sys.Date())

# get the cooresponding data
speech_data <- RtsaPkg::get_series_data(astsa::speech)

# create a data.frame and plot the data
speech_df <- data.frame(
  time = speech_time,
  data = speech_data
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
