library(anytime)
library(data.table)
library(RtsaPkg)

# Create a data.table with year and month columns and
#   their corresponding data values.

# Create a month vector for 11 years.
month <- c()
for(i in 1:11){
  for(ii in 1:12){
    month <- as.character(c(month, ii))
  }
}

# Create a year vector for 11 years.
year <- c()
for(i in 1890:1900) {
  for(ii in 1:12){
    year <- as.character(c(year,i))
  }
}

# Create a data.frame with columns for year, month, day, and
#   values from a random normal distribution.
dt <- data.table::data.table(
  year = year,
  month = month,
  day = '01',
  values = rnorm(12*11, 17, 8)
)

# Using RtsaPkg::add_date_column(), add a Date class column
#   to dt and name it "time".
dt <- RtsaPkg::add_date_column(
  dt = dt,
  time_columns = c("year", "month", "day"),
  column_name = "time"
)

# Show dt.
head(dt)

# Show the class of "time" column as Date
class(dt$time)