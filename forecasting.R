install.packages("forecast")
install.packages("Metrics")
library(forecast)
library(Metrics)
library(psych)
library(tseries)

# Data time series
data_values <- c(
  100313295392, 116472593777, 147011581389, 180531525336, 122436939713,
  137234051842, 117820931721, 131571832503, 120299608578, 128924084424,
  118282949800, 126513693031, 133393213158, 160702896750, 160697361476,
  183435408538, 191888457681, 187332854149, 177787741553, 186754720197,
  203708637108, 195184927132, 197993666182, 207073586073, 198841733913,
  205082988760, 220404504993, 240875090487, 228426121594, 245650165294,
  260712286580, 225237672357, 225841409332, 273924920264, 230616636208,
  225000000000
)

ts_data <- ts(data_values, start = c(2022, 1), frequency = 12)
describe(ts_data)

plot(ts_data, main = "Plot Time Series Belanja Barang", ylab = "Nilai", xlab = "Tahun", col = "blue")
dekomposisi_add <- decompose(ts_data, type = "additive")
plot(dekomposisi_add)

# Split data
train_ts <- window(ts_data, end = c(2024, 7))  # 31 bulan
test_ts <- window(ts_data, start = c(2024, 8))  # 5 bulan

# Model aditif
hwa_train <- HoltWinters(train_ts, seasonal = "additive")
hwa_train
#forecast
forecast_hwa <- forecast(hwa_train, h = length(test_ts))
forecast_hwa
print(hwa_train$fitted)

# Model multiplikatif
options(scipen = 999)
hwm_train <- HoltWinters(train_ts, seasonal = "multiplicative")
hwm_train
#forecast
forecast_hwm <- forecast(hwm_train, h = length(test_ts))
forecast_hwm
print(hwm_train$fitted)

# Fitted values
fitted_hwa_train <- hwa_train$fitted[,1]
fitted_hwm_train <- hwm_train$fitted[,1]
train_ts_fitted <- window(train_ts, start = start(train_ts)[1] + 1/12)


# MAPE
mape_hwa_train <- mape(train_ts_fitted, fitted_hwa_train) * 100
mape_hwa_train
mape_hwm_train <- mape(train_ts_fitted, fitted_hwm_train) * 100
mape_hwm_train
mape_hwa_test <- mape(test_ts, forecast_hwa$mean) * 100
mape_hwa_test
mape_hwm_test <- mape(test_ts, forecast_hwm$mean) * 100
mape_hwm_test

cat("MAPE TRAINING (Aditif):", round(mape_hwa_train, 2), "%\n")
cat("MAPE TESTING (Aditif):", round(mape_hwa_test, 2), "%\n\n")
cat("MAPE TRAINING (Multiplikatif):", round(mape_hwm_train, 2), "%\n")
cat("MAPE TESTING (Multiplikatif):", round(mape_hwm_test, 2), "%\n")

# Menentukan model terbaik berdasarkan MAPE testing terkecil
if (mape_hwa_test < mape_hwm_test) {
  model_terbaik <- HoltWinters(window(ts_data, end = c(2024, 12)), seasonal = "additive")
  forecast_12bulan <- forecast(model_terbaik, h = 12)
  cat("Model terbaik berdasarkan MAPE terkecil: Aditif\n")
  cat("MAPE Testing (Aditif):", round(mape_hwa_test, 2), "%\n")
} else {
  model_terbaik <- HoltWinters(window(ts_data, end = c(2024, 12)), seasonal = "multiplicative")
  forecast_12bulan <- forecast(model_terbaik, h = 12)
  cat("Model terbaik berdasarkan MAPE terkecil: Multiplikatif\n")
  cat("MAPE Testing (Multiplikatif):", round(mape_hwm_test, 2), "%\n")
}

# hasil forecast 12 bulan ke depan
print(forecast_12bulan)
plot(forecast_12bulan, main = "Forecast 12 Periode ke Depan Berdasarkan Model Terbaik", col = "darkgreen")

# Plot training vs fitted
plot(train_ts, type = "l", col = "red", lwd = 2,
     ylim = range(c(train_ts, fitted_hwa_train, fitted_hwm_train)),
     ylab = "Nilai", xlab = "Tahun", main = "Perbandingan Training vs Fitted")
lines(train_ts_fitted, col = "red", lwd = 2)
lines(ts(fitted_hwa_train, start = time(train_ts_fitted)[1], frequency = 12),
      col = "green3", lwd = 2, lty = 2)
lines(ts(fitted_hwm_train, start = time(train_ts_fitted)[1], frequency = 12),
      col = "blue", lwd = 2, lty = 3)
legend("topleft", legend = c("Aktual (Training)", "Fitted Aditif", "Fitted Multiplikatif"),
       col = c("red", "green3", "blue"), lty = c(1, 2, 3), lwd = 2)

# Plot testing vs forecast
y_range <- range(c(test_ts, forecast_hwa$mean, forecast_hwm$mean))
y_range <- c(y_range[1] * 0.9, y_range[2] * 1.1)

plot(test_ts, type = "l", col = "red", lwd = 2, ylim = y_range,
     ylab = "Nilai", xlab = "Tahun",
     main = "Perbandingan Testing vs Forecast Aditif & Multiplikatif")
lines(forecast_hwa$mean, col = "green3", lwd = 2, lty = 2)
lines(forecast_hwm$mean, col = "blue", lwd = 2, lty = 3)
lines(test_ts, col = "red", type = "b", lwd = 2)  # "b" = both points and lines
legend("topleft", legend = c("Aktual (Testing)", "Forecast Aditif", "Forecast Multiplikatif"),
       col = c("red", "green3", "blue"), lty = c(1, 2, 3), lwd = 2)

