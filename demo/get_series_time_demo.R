library(zoo)
library(xts)
library(astsa)
library(data.table)
library(RtsaPkg)

# Get the Date time component of monthly live births from 1948 to 1979
birth_time_Date <- RtsaPkg::get_series_time(series = astsa::birth)
class(birth_time)

# Get the yearmon time component of monthly live births from 1948 to 1979
birth_time_yearmon <- RtsaPkg::get_series_time(
  series = astsa::birth, time_class = "yearmon")
class(birth_time_month_yr)
