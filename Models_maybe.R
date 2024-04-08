
#TSLM LAG

library(forecast)

total_reviews$sum_reviews <- total_reviews$entries.x + total_reviews$entries.y + total_reviews$entries


total_reviews$lag_1 <- lag(total_reviews$sum_reviews, 1)
total_reviews$lag_2 <- lag(total_reviews$sum_reviews, 2)
total_reviews$lag_3 <- lag(total_reviews$sum_reviews, 3)

total_reviews <- na.omit(total_reviews)


train_data <- total_reviews[1:(nrow(total_reviews) - 12), ] 
test_data <- total_reviews[(nrow(total_reviews) - 11):nrow(total_reviews), ] 

model1 <- lm(sum_reviews ~ lag_1 + lag_2 + lag_3, data = train_data)

predictions1 <- predict(model, newdata = test_data)

summary(model1)
AIC(model1)
print(predictions1)

#-----------------------------------------------------------------------------------------------------

#EXPONENTIAL SMOOTHING

ts_data <- ts(total_reviews$sum_reviews, frequency = 12)

ets_model <- ets(ts_data)

forecast_ets <- forecast(ets_model, h = 12)

print(forecast_ets)

summary(ets_model)

#-----------------------------------------------------------------------------------------------------

#ARIMA

ts_data <- ts(total_reviews$sum_reviews, frequency = 12)

arima_model <- auto.arima(ts_data)

forecast_arima <- forecast(arima_model, h = 12)

print(forecast_arima)

summary(arima_model)

#-----------------------------------------------------------------------------------------------------

#SARIMA

sarima_model <- auto.arima(ts_data, seasonal = TRUE)

forecast_sarima <- forecast(sarima_model, h = 12)

print(forecast_sarima)

summary(sarima_model)

#-----------------------------------------------------------------------------------------------------

#STL

stl_result <- stl(ts_data, s.window = "periodic")

trend <- stl_result$time.series[, "trend"]

seasonal <- stl_result$time.series[, "seasonal"]

combined_forecast <- trend + seasonal

forecast_stl <- tail(combined_forecast, 12)

print(forecast_stl)

summary(stl_result)

#TRIED AND FAILED TO CALC AIC
sum_sq_residuals <- sum(stl_result$time.series[, "remainder"]^2)

num_params <- length(stl_result$time.series[, "trend"]) + length(stl_result$time.series[, "seasonal"])

aic_like <- -2 * log(sum_sq_residuals) + 2 * num_params

print(aic_like)

#-----------------------------------------------------------------------------------------------------
