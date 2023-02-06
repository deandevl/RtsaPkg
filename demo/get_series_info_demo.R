library(zoo)
library(xts)
library(astsa)
library(data.table)
library(here)
library(RtsaPkg)

# Information on chicken prices 'ts' single variable time series
chicken_ts_prices_info_lst <- RtsaPkg::get_series_info(series = astsa::chicken, n_obser = NULL)

# Information on a multivariate time series with 260 daily closing prices sampled per year from
#   the middle of 1991 (cycle = 130) to 3/4's of 1998 (cycle = 169) There are
#   1860 total sampled points for 4 variables of major European stock indices
#   (Germany DAX, Switzerland SMI, France CAC, UK FTSE).
EURO_Stock_Market_info_lst <- RtsaPkg::get_series_info(series = datasets::EuStockMarkets)

# Information on consumer sentiment index of the University of Michigan since 1980. This is
# a single variable 'xts' time series.
mich_data_path <- file.path(here::here(), "demo", "data", "Michigan_CS_xts.rda")
load(mich_data_path)
mich_info_lst <- RtsaPkg::get_series_info(series = Michigan_CS)

# Information on crude oil prices in Europe. This is a single variable 'zoo' time series.
EURO_Brent_data_path <- file.path(here::here(), "demo", "data", "EURO_Brent_zoo.rda")
load(EURO_Brent_data_path)
EURO_Brent_info_lst <- RtsaPkg::get_series_info(series = EURO_Brent)


