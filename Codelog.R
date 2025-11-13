data <- read.csv("streamgage_07374000_20230519.csv")

data <- na.omit(data)

# Filter to include only approved (A) gage height readings
data <- subset(data, gage_height_quality == "A")

# Remove anomalies (negative or zero values that shouldn't exist in streamflow)
data <- subset(data, discharge > 0 & gage_height > 0)

# Outlier detection using z-scores (flag values beyond 3 standard deviations)
data$discharge_z <- scale(data$discharge)
data$gage_height_z <- scale(data$gage_height)

# Remove outliers
data <- subset(data, abs(discharge_z) <= 3 & abs(gage_height_z) <= 3)

# Drop z-score columns and unnecessary columns
data <- subset(data, select = -c(discharge_z, gage_height_z, agency, timezone))


library(ggplot2)
data <- read.csv("streamgage_07374000_20230519.csv")


#data_plot <- subset(data, datetime >= as.POSIXct("2009-01-01") & datetime <  as.POSIXct("2010-01-01"))
data_plot <- data   # plot everything

#–––  SCALE DISCHARGE WITH GAGE HEIGHT  ––
# We put discharge on a secondary axis by converting it to the same
# numeric range as gage_height (simple min-max rescaling).
rng_height   <- range(data_plot$gage_height, na.rm = TRUE)
rng_discharge <- range(data_plot$discharge,  na.rm = TRUE)

scale_discharge <- function(x) {
  (x - rng_discharge[1]) /
    diff(rng_discharge) * diff(rng_height) + rng_height[1]
}

data_plot$discharge_scaled <- scale_discharge(data_plot$discharge)

#–––  PLOT  ––––––––––––––––––––––––––––––––––
ggplot(data_plot, aes(x = as.POSIXct(datetime))) +
  geom_line(aes(y = gage_height,            colour = "Gage Height (ft)"), size = 0.4) +
  geom_line(aes(y = discharge_scaled,       colour = "Discharge (cfs)"), size = 0.4) +
  scale_y_continuous(
    name = "Gage Height (ft)",
    sec.axis = sec_axis(~ (.-rng_height[1]) / diff(rng_height) *
                          diff(rng_discharge) + rng_discharge[1],
                        name = "Discharge (cfs)")
  ) +
  scale_colour_manual(values = c("Gage Height (ft)" = "steelblue",
                                 "Discharge (cfs)"  = "firebrick")) +
  labs(title = "Mississippi River at Baton Rouge (USGS 07374000)",
       x = "Date / Time", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom")