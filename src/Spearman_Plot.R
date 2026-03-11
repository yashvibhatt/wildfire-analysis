# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(corrplot)

# Load the cleaned dataset
fires <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)
fires$ALARM_DATE <- as.Date(fires$ALARM_DATE)

# Select all relevant numeric variables
cor_data <- fires %>%
  select(Shape__Area, FIRE_DURATION, Temperature_C, MaxTemperature_C, WindSpeed_kmh, MaxWindSpeed_kmh) %>%
  na.omit()  # Remove rows with missing values

# Compute Spearman correlation matrix
pearson_matrix <- cor(cor_data, method = "pearson")

# Print the correlation matrix
print(pearson_matrix)

# Plot the correlation matrix
corrplot(pearson_matrix, method = "color", type = "upper",
         tl.col = "black", addCoef.col = "black",
         title = "Pearson Rank Correlation Matrix (with Max Temp & Wind)",
         mar=c(0,0,1,0))
df <- df[complete.cases(df[, c("FIRE_DURATION", "Temperature_C", "MaxTemperature_C",
                               "WindSpeed_kmh", "MaxWindSpeed_kmh")]), ]

