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

# Monthly fire trends
fires %>%
  mutate(month = month(ALARM_DATE, label = TRUE)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Wildfires Per Month", x = "Month", y = "Number of Fires") +
  theme_minimal() +
  theme(legend.position = "none")