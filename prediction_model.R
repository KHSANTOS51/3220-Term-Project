library(ggplot2)
library(forecast)

# ------------------------
# Data preparation
# ------------------------

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

# ------------------------
# Exploratory visualization
# ------------------------

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
    title = "Mississippi River at Baton Rouge (USGS 07374000)",
    subtitle = "Year-long gage height and discharge (dual axis)",
    x = "Date / Time"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(dual_axis_plot)

set.seed(42)
sample_size <- min(20000, nrow(gage_data)) # this is so that it doesn't take forever to render / load
scatter_data <- gage_data[sample.int(nrow(gage_data), sample_size), ]

scatter_plot <- ggplot(scatter_data, aes(x = gage_height, y = discharge)) +
  geom_point(alpha = 0.25, colour = "steelblue", size = 0.6) +
  labs(
    title = "Discharge vs gage height (20k-point sample)",
    subtitle = "Identifies the rating-curve relationship",
    x = "Gage Height (ft)",
    y = "Discharge (cfs)"
  ) +
  theme_minimal()

print(scatter_plot)

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

# ------------------------
# Seasonal decomposition
# ------------------------

gage_data$date <- as.Date(gage_data$datetime)
daily_data <- aggregate(cbind(discharge, gage_height) ~ date, data = gage_data, mean)
daily_data <- daily_data[order(daily_data$date), ]
message(sprintf("daily rows: %s", nrow(daily_data)))

series_frequency <- 365
ts_discharge <- ts(daily_data$discharge, frequency = series_frequency)
ts_gage_height <- ts(daily_data$gage_height, frequency = series_frequency)

discharge_decomp <- decompose(ts_discharge, type = "additive")
gage_height_decomp <- decompose(ts_gage_height, type = "additive")
par(mfrow = c(4, 1), oma = c(2, 0, 2, 0), mar = c(2, 4, 1, 1))
plot(discharge_decomp)
mtext("Date", side = 1, line = 2)
mtext("Additive decomposition: discharge", side = 3, outer = TRUE, line = 0.5)
par(mfrow = c(4, 1), oma = c(2, 0, 2, 0), mar = c(2, 4, 1, 1))
plot(gage_height_decomp)
mtext("Date", side = 1, line = 2)
mtext("Additive decomposition: gage height", side = 3, outer = TRUE, line = 0.5)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)

discharge_stl <- stl(ts_discharge, s.window = "periodic")
gage_height_stl <- stl(ts_gage_height, s.window = "periodic")
par(mfrow = c(4, 1), oma = c(2, 0, 2, 0), mar = c(2, 4, 1, 1))
plot(discharge_stl, main = "")
mtext("Date", side = 1, line = 2)
mtext("STL decomposition: discharge", side = 3, outer = TRUE, line = 0.5)
par(mfrow = c(4, 1), oma = c(2, 0, 2, 0), mar = c(2, 4, 1, 1))
plot(gage_height_stl, main = "")
mtext("Date", side = 1, line = 2)
mtext("STL decomposition: gage height", side = 3, outer = TRUE, line = 0.5)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)

discharge_adjusted <- seasadj(discharge_stl)
gage_height_adjusted <- seasadj(gage_height_stl)

# ------------------------
# ARIMA / SARIMA modeling
# ------------------------

total_obs <- length(ts_discharge)
train_size <- floor(0.8 * total_obs)
holdout_size <- total_obs - train_size
message(sprintf("train: %s | holdout: %s", train_size, holdout_size))

discharge_train <- window(discharge_adjusted, end = c(0, train_size))
discharge_test <- window(discharge_adjusted, start = c(0, train_size + 1))
discharge_test_vec <- as.numeric(discharge_test)
gage_height_reg_train <- window(gage_height_adjusted, end = c(0, train_size))
gage_height_reg_test <- window(gage_height_adjusted, start = c(0, train_size + 1))
gage_height_reg_train_vec <- as.numeric(gage_height_reg_train)
gage_height_reg_test_vec <- as.numeric(gage_height_reg_test)

discharge_model <- auto.arima(
  discharge_train,
  xreg = gage_height_reg_train_vec,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = FALSE
)

discharge_label <- format_arima_label(discharge_model)
message(sprintf("discharge model (with gage height regressor): %s", discharge_label))

forecast_discharge <- forecast(
  discharge_model,
  xreg = gage_height_reg_test_vec
)

discharge_forecast_plot <- autoplot(forecast_discharge) +
  autolayer(discharge_test, series = "Actual") +
  labs(
    title = "Discharge forecast",
    subtitle = paste(discharge_label, "+ gage-height regressor"),
    x = "Time",
    y = "Discharge (cfs)"
  ) +
  theme_minimal()

print(discharge_forecast_plot)

discharge_accuracy <- accuracy(forecast_discharge, discharge_test)
message(sprintf("discharge RMSE: %.2f", discharge_accuracy[2, "RMSE"]))

# ------------------------
# Model evaluation
# ------------------------

discharge_eval <- evaluate_forecast(discharge_test, forecast_discharge$mean)
evaluation_summary <- data.frame(
  series = "Discharge",
  rmse = discharge_eval$rmse,
  mae = discharge_eval$mae,
  r_squared = discharge_eval$r_squared
)

message("evaluation metrics (RMSE for disaster readiness, MAE for daily reporting, R-squared for pattern capture):")
print(evaluation_summary)

# ------------------------
# Diagnostics and summary
# ------------------------

checkresiduals(discharge_model)
discharge_lb <- Box.test(residuals(discharge_model), lag = 20, type = "Ljung-Box")
message(sprintf("discharge Ljung-Box p = %.3f", discharge_lb$p.value))