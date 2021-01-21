library(RtsaPkg)

# simple single date conversion that follows the ISO 8601 format
my_date <- RtsaPkg::get_Date("2020-03-12")

# simple single date conversion that does not follow ISO 8601 format
my_date <- RtsaPkg::get_Date("January 21, 2017", date_format = "%B %d, %Y")

# a vector of character strings
dates <- c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01")
my_dates <- RtsaPkg::get_Date(dates)