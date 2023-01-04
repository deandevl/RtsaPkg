library(zoo)
library(xts)
library(astsa)
library(RtsaPkg)

# Information on single series on chicken prices
chicken_ts_prices_info <- RtsaPkg::get_series_info(series = astsa::chicken)

# Information on a multivariate with 260 data points sampled per year from
#   the middle of 1991 (cycle = 130) to 3/4's of 1998 (cycle = 169) There are
#   1860 total sampled points per variable.
EURO_Stock_Market_info <- RtsaPkg::get_series_info(series = datasets::EuStockMarkets)
