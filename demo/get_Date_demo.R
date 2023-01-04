library(RtsaPkg)

# Simple single date string to Date class conversion that follows
#   the ISO 8601 format.
my_date_1 <- RtsaPkg::get_Date(date_vals = "2020-03-12")
print(my_date_1)
class(my_date_1)

# Simple single date string to Date class conversion that
#   does not follow ISO 8601 format. The date_format parameter
#   is specified.
my_date_2 <- RtsaPkg::get_Date(
  date_vals = "January 21, 2017",
  date_format = "%B %d, %Y"
)
print(my_date_2)

# A vector of character string dates to be converted.
my_dates_v <- c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01")
my_dates <- RtsaPkg::get_Date(my_dates_v)
print(my_dates)
class(my_dates[[3]])