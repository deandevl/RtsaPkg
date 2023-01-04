library(data.table)
library(magrittr)
library(RtsaPkg)

ar_2_ts <- stats::arima.sim(model = list(ar = c(0.6, 0.3)), n=100)
ar_2_dt <- RtsaPkg::tsObj_to_dt(ar_2_ts)
str(ar_2_dt)

lag_1 <- data.table::shift(ar_2_dt, n = 1)[[2]]
lag_2 <- data.table::shift(ar_2_dt, n = 2)[[2]]

ar_2_lags <- ar_2_dt %>%
  .[, .(
    series_value = value,
    lag_1 = data.table::shift(., n = 1)[[2]],
    lag_2 = data.table::shift(., n = 2)[[2]]
  )]

# using stats::lm to fit the AR(2) model
fit_ols_ar2 <- stats::lm(series_value ~ lag_1 + lag_2, data = ar_2_lags)
summary(fit_ols_ar2)

# using stats::arima to fit AR(2)
fit_arima_ar2 <- stats::arima(x = ar_2_ts, order = c(2,0,0), include.mean = TRUE, method = "CSS")
fit_arima_ar2

value_mean <- mean(ar_2_dt$value)
arima_coef <- coef(fit_arima_ar2)

ols_intercept <- arima_coef[3] * (1 - arima_coef[1] - arima_coef[2])

#using stats::ar.ols
fit_ar_ols_ar2 <- stats::ar.ols(x = ar_2_ts, domean = TRUE, intercept = TRUE)

