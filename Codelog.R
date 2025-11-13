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