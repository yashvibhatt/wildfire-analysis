# Install and load necessary packages
#install.packages(c("ggplot2", "dplyr", "lubridate", "sf", "ggmap"))
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(ggmap)

# 🔹 Load the cleaned dataset
fires <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)

# Convert ALARM_DATE to Date format
fires$ALARM_DATE <- as.Date(fires$ALARM_DATE, format="%Y-%m-%d")


# Fire Duration Distribution
ggplot(fires, aes(x = FIRE_DURATION)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(title = "Fire Duration Distribution", x = "Duration (Days)", y = "Count") +
  theme_minimal()

