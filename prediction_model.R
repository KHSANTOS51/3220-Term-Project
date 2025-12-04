# ============================================================================                                                      
# ARIMA/SARIMA time-series workflow for USGS 07374000 (Baton Rouge)
# ============================================================================

library(ggplot2)
library(forecast)

# -----------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------

clean_streamgage_data <- function(path) {
  data <- read.csv(path)
  data <- na.omit(data)
  data$datetime <- as.POSIXct(data$datetime)
  data <- data[order(data$datetime), ]
  data <- subset(data, gage_height_quality == "A")
  data <- subset(data, discharge > 0 & gage_height > 0)
  data$discharge_z <- scale(data$discharge)
  data$gage_height_z <- scale(data$gage_height)
  data <- subset(data, abs(discharge_z) <= 3 & abs(gage_height_z) <= 3)
  subset(data, select = -c(discharge_z, gage_height_z, agency, timezone))
}

gage_data <- clean_streamgage_data("streamgage_07374000_20230519.csv")
message(
  sprintf(
    "clean rows: %s | range: %s - %s",
    nrow(gage_data),
    format(min(gage_data$datetime)),
    format(max(gage_data$datetime))
  )
)

# -----------------------------------------------------------------------------
# Exploratory visualization (Dual Axis Only)
# -----------------------------------------------------------------------------

scale_discharge_to_height <- function(discharge, height) {
  height_range <- range(height, na.rm = TRUE)
  discharge_range <- range(discharge, na.rm = TRUE)
  scaled <- (discharge - discharge_range[1]) /
    diff(discharge_range) * diff(height_range) + height_range[1]
  list(
    scaled = scaled,
    height_range = height_range,
    discharge_range = discharge_range
  )
}

# Always plot dual-axis chart (1-year window for clarity)
plot_window <- subset(
  gage_data,
  datetime >= max(gage_data$datetime) - 365 * 24 * 60 * 60
)
if (nrow(plot_window) == 0) plot_window <- gage_data
scaling <- scale_discharge_to_height(plot_window$discharge, plot_window$gage_height)
plot_window$discharge_scaled <- scaling$scaled

dual_axis_plot <- ggplot(plot_window, aes(x = datetime)) +
  geom_line(aes(y = gage_height, colour = "Gage Height (ft)"), linewidth = 0.4) +
  geom_line(aes(y = discharge_scaled, colour = "Discharge (cfs)"), linewidth = 0.4) +
  scale_y_continuous(
    name = "Gage Height (ft)",
    sec.axis = sec_axis(
      ~ (. - scaling$height_range[1]) / diff(scaling$height_range) *
        diff(scaling$discharge_range) + scaling$discharge_range[1],
      name = "Discharge (cfs)"
    )
  ) +
  scale_colour_manual(
    values = c("Gage Height (ft)" = "steelblue", "Discharge (cfs)" = "firebrick"),
    guide = guide_legend(title = NULL)
  ) +
  labs(
    title = "Dual-Axis Plot",
    subtitle = "Gage Height vs Discharge (Last Year Window)",
    x = "Date / Time"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(dual_axis_plot)

format_arima_label <- function(model) {
  if (!is.null(model$method) && nzchar(model$method)) {
    return(model$method)
  }
  ord <- arimaorder(model)
  sprintf(
    "ARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
    ord[1], ord[2], ord[3], ord[4], ord[5], ord[6], ord[7]
  )
}

evaluate_forecast <- function(actual, predicted) {
  actual_vec <- as.numeric(actual)
  predicted_vec <- as.numeric(predicted)
  len <- min(length(actual_vec), length(predicted_vec))
  actual_vec <- actual_vec[seq_len(len)]
  predicted_vec <- predicted_vec[seq_len(len)]
  mask <- is.finite(actual_vec) & is.finite(predicted_vec)
  actual_vec <- actual_vec[mask]
  predicted_vec <- predicted_vec[mask]
  rmse <- sqrt(mean((actual_vec - predicted_vec)^2))
  mae <- mean(abs(actual_vec - predicted_vec))
  ss_res <- sum((actual_vec - predicted_vec)^2)
  ss_tot <- sum((actual_vec - mean(actual_vec))^2)
  r_squared <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  list(rmse = rmse, mae = mae, r_squared = r_squared)
}

# -----------------------------------------------------------------------------
# Seasonal decomposition (Preparation only, no plots)
# -----------------------------------------------------------------------------

gage_data$date <- as.Date(gage_data$datetime)
daily_data <- aggregate(cbind(discharge, gage_height) ~ date, data = gage_data, mean)
daily_data <- daily_data[order(daily_data$date), ]
message(sprintf("daily rows: %s", nrow(daily_data)))

series_frequency <- 365
ts_discharge <- ts(daily_data$discharge, frequency = series_frequency)
ts_gage_height <- ts(daily_data$gage_height, frequency = series_frequency)

# Use STL for adjustment
discharge_stl <- stl(ts_discharge, s.window = "periodic")
discharge_adjusted <- seasadj(discharge_stl)
gage_height_adjusted <- seasadj(stl(ts_gage_height, s.window = "periodic"))

# -----------------------------------------------------------------------------
# ARIMA / SARIMA modeling with 80/10/10 Split
# -----------------------------------------------------------------------------

total_obs <- length(ts_discharge)
train_size <- floor(0.8 * total_obs)
val_size <- floor(0.1 * total_obs)
test_size <- total_obs - train_size - val_size

message(sprintf("Split: Train=%d | Val=%d | Test=%d", train_size, val_size, test_size))

# Create windows for Train, Validation, Test
# Using standard indexing to ensure alignment
discharge_train <- window(discharge_adjusted, end = c(1, train_size))
discharge_val   <- window(discharge_adjusted, start = c(1, train_size + 1), end = c(1, train_size + val_size))
discharge_test  <- window(discharge_adjusted, start = c(1, train_size + val_size + 1))

gage_height_train <- window(gage_height_adjusted, end = c(1, train_size))
gage_height_val   <- window(gage_height_adjusted, start = c(1, train_size + 1), end = c(1, train_size + val_size))
gage_height_test  <- window(gage_height_adjusted, start = c(1, train_size + val_size + 1))

# Convert regressor series to numeric vectors for ARIMA
xreg_train <- as.numeric(gage_height_train)
xreg_val   <- as.numeric(gage_height_val)
xreg_test  <- as.numeric(gage_height_test)

# 1. Train Model on 80%
discharge_model <- auto.arima(
  discharge_train,
  xreg = xreg_train,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = FALSE
)

discharge_label <- format_arima_label(discharge_model)
message(sprintf("Discharge model (Train 80%%): %s", discharge_label))

# 2. Validate on next 10%
forecast_val <- forecast(discharge_model, xreg = xreg_val)
# Evaluation: Validation Set
message("\n--- Validation Set Performance (10%) ---")
val_eval <- evaluate_forecast(discharge_val, forecast_val$mean)
print(data.frame(RMSE=val_eval$rmse, MAE=val_eval$mae, R2=val_eval$r_squared))

# 3. Test on final 10%
# Fix: Re-fit on Train + Validation without re-estimation logic error
# We use the model structure from training but update coefficients on 90% of data
discharge_train_val <- window(discharge_adjusted, end = c(1, train_size + val_size))
xreg_train_val <- c(xreg_train, xreg_val)

# Ensure lengths match perfectly before refitting
len_combined <- length(discharge_train_val)
xreg_train_val <- xreg_train_val[1:len_combined]

discharge_model_final <- Arima(
  discharge_train_val,
  model = discharge_model, 
  xreg = xreg_train_val
)

forecast_test <- forecast(discharge_model_final, xreg = xreg_test)

message("\n--- Test Set Performance (Final 10%) ---")
test_eval <- evaluate_forecast(discharge_test, forecast_test$mean)
print(data.frame(RMSE=test_eval$rmse, MAE=test_eval$mae, R2=test_eval$r_squared))

# Plot Test Forecast vs Actuals (Test Set Only)
test_dates <- seq_along(discharge_test)
forecast_df <- data.frame(
  time = test_dates,
  actual = as.numeric(discharge_test),
  forecast = as.numeric(forecast_test$mean),
  lower_80 = as.numeric(forecast_test$lower[, 1]),
  upper_80 = as.numeric(forecast_test$upper[, 1]),
  lower_95 = as.numeric(forecast_test$lower[, 2]),
  upper_95 = as.numeric(forecast_test$upper[, 2])
)

discharge_forecast_plot <- ggplot(forecast_df, aes(x = time)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.4) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = forecast, colour = "Forecast"), linewidth = 0.8) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.8) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  labs(
    title = "Forecast vs Actuals",
    subtitle = paste(discharge_label, "+ gage-height regressor (Test Set Only)"),
    x = "Day (Test Period)",
    y = "Discharge (cfs)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(discharge_forecast_plot)

# -----------------------------------------------------------------------------
# Diagnostics and summary
# -----------------------------------------------------------------------------

# Only print Ljung-Box text output for diagnostics
discharge_lb <- Box.test(residuals(discharge_model), lag = 20, type = "Ljung-Box")
message(sprintf("discharge Ljung-Box p = %.3f", discharge_lb$p.value))

message("data checks: z-score filter, A flag only, positive flows")
message("next steps: alternative anomaly detection, external regressors")
message("workflow complete")
