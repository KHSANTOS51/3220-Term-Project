data <- read.csv("streamgage_07374000_20230519.csv")

data <- na.omit(data)

# Filter to include only approved (A) gage height readings
data <- subset(data, gage_height_quality == "A")