library(dplyr)
library(RtsaPkg)

ar_2_ts <- stats::arima.sim(model = list(ar = c(0.6, 0.3)), n=100)
ar_2_df <- RtsaPkg::ts_to_df(ar_2_ts)
str(ar_2_df)

ar_2_lags <- ar_2_df %>%
  mutate(
    lag_1 = dplyr::lag(Value,1),
    lag_2 = dplyr::lag(Value,2)
  ) %>%
  na.omit()

# using stats::lm to fit the AR(2) model
fit_ols_ar2 <- stats::lm(Value ~ lag_1 + lag_2, data = ar_2_lags)
summary(fit_ols_ar2)

# using stats::arima to fit AR(2)
fit_arima_ar2 <- stats::arima(x = ar_2_ts, order = c(2,0,0), include.mean = TRUE, method = "CSS")
fit_arima_ar2

Value_mean <- mean(ar_2_df$Value)
arima_coef <- coef(fit_arima_ar2)

ols_intercept <- arima_coef[3] * (1 - arima_coef[1] - arima_coef[2])

#using stats::ar.ols
fit_ar_ols_ar2 <- stats::ar.ols(x = ar_2_ts, domean = TRUE, intercept = TRUE)


