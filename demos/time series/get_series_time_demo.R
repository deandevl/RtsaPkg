library(zoo)
library(xts)
library(TSstudio)
library(RtsaPkg)

# get the time component of a 'ts' time object as a Date object (the default)
RtsaPkg::get_series_info(TSstudio::USgas)
USgas_Date <- RtsaPkg::get_series_time(series = TSstudio::USgas)
head(USgas_Date)

# get the time component of a ts time object as a yearmon object
USgas_yearmon <- RtsaPkg::get_series_time(TSstudio::USgas, time_class = "yearmon")
head(USgas_yearmon)

# get the time component of a ts time object as a yearqtr object
USgas_yearqtr <- RtsaPkg::get_series_time(TSstudio::USgas, time_class = "yearqtr")
head(USgas_yearqtr)

# get the index of zoo time object EURO_Brent as a Date object
RtsaPkg::get_series_info(TSstudio::EURO_Brent)
EURO_Brent_Date <- RtsaPkg::get_series_time(TSstudio::EURO_Brent)
head(EURO_Brent_Date)

# get the index of zoo time object EURO_Brent as a yearqtr object
EURO_Brent_yearqtr <- RtsaPkg::get_series_time(TSstudio::EURO_Brent, time_class = "yearqtr")
str(EURO_Brent_yearqtr)

# get the index of Michigan_CS xts object as a Date object
RtsaPkg::get_series_info(series = TSstudio::Michigan_CS)
Michigan_CS_Date <- RtsaPkg::get_series_time(TSstudio::Michigan_CS)
str(Michigan_CS_Date)

# Looking at astsa::speech with a time index of a .1 second
# convert index to POSIXct of astsa::speech
RtsaPkg::get_series_info(series = astsa::speech)

# get time index and convert to actual time units of tenth of second
speech_index <- zoo::index(astsa::speech)/10

# create a POSIXct time object from speech_index
speech_time <- as.POSIXct(speech_index, origin = Sys.Date())

# get data and create a data frame of time and data
speech_data <- zoo::coredata(astsa::speech)
speech_df <- data.frame(
  Date = speech_time,
  Data = speech_data
)

# convert data frame to "xts" object
speech_xts <- RtsaPkg::df_to_xts(speech_df, time_col = "Date", data_cols = "Data")
# get the index of the "xts" object
speech_xts_index <- RtsaPkg::get_series_time(speech_xts, time_class = "POSIXct")



