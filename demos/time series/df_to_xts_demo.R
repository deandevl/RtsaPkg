library(xts)
library(TSstudio)
library(RtsaPkg)

# convert US_indicators data set to an xts time series object
str(TSstudio::US_indicators)

US_indicators_xts <- RtsaPkg::df_to_xts(
  TSstudio::US_indicators,
  time_col = "Date",
  data_cols = c("Vehicle Sales", "Unemployment Rate"))

TSstudio::ts_info(US_indicators_xts)
