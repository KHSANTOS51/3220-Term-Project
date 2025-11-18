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

data <- subset(data, select = -c((id))

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



#---------Null Values-------------#

#all rows where the gage height is null
null_rows <- df[is.na(df$gage_height), ]

#all rows where the gage quality height is null
null_quality <- df[is.na(df$gage_height_quality), ]

#all rows where both gage quality and height are null
null_both <- df[is.na(df$gage_height) & is.na(df$gage_height_quality), ]

library(dplyr)
library(ggplot2)

# If you already have df loaded, skip this
df <- read.csv("streamgage_07374000_20230519.csv")

# If you already converted to UTC, use that column instead.
# Otherwise, make datetime POSIXct (assumes it's in local time).
df$datetime <- as.POSIXct(df$datetime, tz = "UTC")

# Keep only rows where gage_height is NA, ordered by time
null_rows <- df %>%
  filter(is.na(gage_height)) %>%
  arrange(datetime)

null_range <- null_rows %>%
  summarise(
    first_null = min(datetime),
    last_null  = max(datetime)
  )

null_range

null_range <- null_rows %>%
  summarise(
    first_null = min(datetime),
    last_null  = max(datetime)
  )

null_range

interval <- 15 * 60  # 15 minutes in seconds

null_rows <- null_rows %>%
  mutate(
    # time difference in seconds from previous null-row
    diff_sec = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
    # new block starts if: first row (is.na(diff_sec)) OR gap > interval
    new_block = if_else(is.na(diff_sec) | diff_sec > interval, 1L, 0L),
    # cumulative sum of new_block flags creates block IDs: 1,2,3,...
    block = cumsum(new_block)
  )

block_summary <- null_rows %>%
  group_by(block) %>%
  summarise(
    start_time     = min(datetime),
    end_time       = max(datetime),
    n_points       = n(),                       # number of null rows in block
    first_discharge = first(discharge),
    last_discharge  = last(discharge),
    min_discharge   = min(discharge, na.rm = TRUE),
    max_discharge   = max(discharge, na.rm = TRUE),
    mean_discharge  = mean(discharge, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(start_time)

block_summary

ggplot(null_rows, aes(x = datetime, y = discharge, colour = factor(block))) +
  geom_point(alpha = 0.6, size = 1) +
  labs(
    colour = "Block",
    x = "Time",
    y = "Discharge",
    title = "Discharge values during periods with null gage_height"
  )

sum(block_summary$n_points == 1) 
