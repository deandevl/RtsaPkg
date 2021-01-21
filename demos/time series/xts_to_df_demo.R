library(data.table)
library(xts)
library(RtsaPkg)
library(TSstudio)

# convert a univariate "xts" object TSstudio::EURO_Brent
# by default the time index will be converted to "Date"
TSstudio::ts_info(TSstudio::Michigan_CS)
xts::tclass(TSstudio::Michigan_CS)
head(TSstudio::Michigan_CS)

col_names <- c("Date", "Sentiment")
Michigan_CS_dt <- RtsaPkg::xts_to_df(TSstudio::Michigan_CS, col_names = col_names)
str(Michigan_CS_dt)
head(Michigan_CS_dt)

# convert a multivariate "xts" object
US_indicators_xts <- RtsaPkg::df_to_xts(
  TSstudio::US_indicators,
  time_col = "Date",
  data_cols = c("Vehicle Sales", "Unemployment Rate"))

TSstudio::ts_info(US_indicators_xts)

US_indicators_dt <- RtsaPkg::xts_to_df(US_indicators_xts)
