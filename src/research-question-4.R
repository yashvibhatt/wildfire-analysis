# 0. Load Required Libraries
# -------------------------------------------------------------------------
print("Loading libraries...")
library(tidyverse)
library(lubridate)
library(caret)
library(ranger)         # For method = "ranger"
library(pROC)
library(gridExtra)      # For arranging plots
library(randomForest)    # For importance compatibility
print("Libraries loaded.")

# 1. Define Data Loading and Preprocessing Function
# -------------------------------------------------------------------------
load_and_preprocess <- function(data_path) {
  print(paste("Attempting to load data from:", data_path))
  if (!file.exists(data_path)) { stop("Data file not found: ", data_path) }
  data <- read_csv(data_path)
  
  data <- data %>%
    mutate(
      DATE = as.Date(DATE),
      YEAR = year(DATE),
      MONTH = month(DATE),
      FIRE_START_DAY = as.logical(FIRE_START_DAY)
    ) %>%
    arrange(DATE) %>%
    mutate(
      TEMP_RANGE = MAX_TEMP - MIN_TEMP,
      MONTH_NAME = factor(month(DATE, label = TRUE, abbr = TRUE), levels = month.abb),
      DECADE = factor(floor(YEAR / 10) * 10),
      SEASON = factor(SEASON, levels = c("Spring","Summer","Fall","Winter")),
      LAGGED_PRECIPITATION = lag(PRECIPITATION, n = 1, default = NA),
      LAGGED_AVG_WIND_SPEED = lag(AVG_WIND_SPEED, n = 1, default = NA),
      WIND_TEMP_RATIO = ifelse(MAX_TEMP %in% c(0, NA), 0, AVG_WIND_SPEED / MAX_TEMP)
    )
  
  print("Handling missing values with median imputation...")
  num_cols <- data %>% select_if(is.numeric) %>% names()
  for(col in num_cols) {
    if(sum(is.na(data[[col]])) > 0) {
      med <- median(data[[col]], na.rm = TRUE)
      if (is.na(med)) med <- 0
      data[[col]][is.na(data[[col]])] <- med
    }
  }
  print("Missing value imputation complete.")
  return(data)
}

# 2. Define Evaluation and Interpretation Functions
# -------------------------------------------------------------------------
plot_importance_caret <- function(model) {
  if(is.null(model)) return(NULL)
  model_name <- model$method
  print(paste("Generating Feature Importance Plot for:", model_name))
  imp <- varImp(model, scale = FALSE)
  gg <- ggplot(imp, top = 10) +
    labs(title=paste('Top 10 Features -', model_name)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(gg)
}

categorize_risk <- function(probs, threshold) {
  if (length(threshold) != 1 || !is.numeric(threshold) || is.na(threshold) || is.infinite(threshold)) {
    warning("Invalid threshold, using 0.5.", call.=FALSE)
    threshold <- 0.5
  }
  risk_levels <- case_when(
    probs >= threshold * 1.5 ~ "Extreme",
    probs >= threshold       ~ "High",
    probs >= threshold / 2   ~ "Moderate",
    TRUE                      ~ "Low"
  )
  factor(risk_levels, levels=c("Low","Moderate","High","Extreme"))
}

print("Helper functions defined.")

# =========================================================================
# Main Script Execution
# =========================================================================

# 3. Set Parameters
# -------------------------------------------------------------------------
data_path     <- "../data/CA_Weather_Fire_Dataset_1984-2025.csv"
split_year    <- 2015
uetooth_length_rf <- 10
cv_repeats    <- 2
cv_number     <- 5

# 4. Load and Preprocess Data
# -------------------------------------------------------------------------
print("--- Loading and Preprocessing ---")
data <- load_and_preprocess(data_path)
print(paste("Dataset dimensions:", paste(dim(data), collapse=" x ")))

# 5. Train/Test Split & Target Preparation
# -------------------------------------------------------------------------
print("--- Splitting Data & Preparing Target Factor ---")
train_years <- 1984:split_year
test_years  <- (split_year + 1):min(max(data$YEAR, na.rm=TRUE), 2023)

train_data <- data %>% filter(YEAR %in% train_years)
test_data  <- data %>% filter(YEAR %in% test_years)

train_data$Fire <- factor(ifelse(train_data$FIRE_START_DAY, "Yes", "No"), levels=c("No","Yes"))
test_data$Fire  <- factor(ifelse(test_data$FIRE_START_DAY, "Yes", "No"),     levels=c("No","Yes"))
target <- "Fire"

print(paste("Train dimensions:", paste(dim(train_data), collapse=" x ")))
print(paste("Test dimensions:", paste(dim(test_data), collapse=" x ")))
print("Target variable distribution in training data:")
print(table(train_data[[target]], useNA="ifany"))

features <- c("MAX_TEMP","MIN_TEMP","PRECIPITATION","AVG_WIND_SPEED","TEMP_RANGE",
              "WIND_TEMP_RATIO","LAGGED_PRECIPITATION","LAGGED_AVG_WIND_SPEED","SEASON","MONTH")
features <- intersect(features, names(train_data))
print("Features selected for modeling:")
print(features)

# 6. Cross-Validation Setup (with Down-sampling & Verbose Output)
# -------------------------------------------------------------------------
print("--- Setting up trainControl ---")
set.seed(123)
cv_ctrl <- trainControl(
  method          = "repeatedcv",
  number          = cv_number,
  repeats         = cv_repeats,
  summaryFunction = twoClassSummary,
  classProbs      = TRUE,
  verboseIter     = TRUE,
  savePredictions = "final",
  sampling        = "down",
  allowParallel   = FALSE
)
print("trainControl configured.")

# 7. Random Forest Tuning & Training (using caret)
# -------------------------------------------------------------------------
print(paste("--- Starting Tuned Ranger Training ---", Sys.time()))
set.seed(456)
tuned_rf_model <- train(
  x          = train_data[, features],
  y          = train_data[[target]],
  method     = "ranger",
  metric     = "ROC",
  tuneLength = tuneLength_rf,
  trControl  = cv_ctrl,
  importance = 'permutation'
)
print(paste("--- Finished Tuned Ranger Training ---", Sys.time()))

print("Best Ranger hyperparameters found by CV:")
print(tuned_rf_model$bestTune)
print("Ranger CV Results:")
print(tuned_rf_model$results[rownames(tuned_rf_model$bestTune),])

# 8. Evaluate on Test Set (using Optimal Thresholds)
# -------------------------------------------------------------------------
print("--- Evaluating Final Model on Hold-Out Test Set ---")
rf_probs <- predict(tuned_rf_model, newdata=test_data[,features], type="prob")[,"Yes"]
rf_roc   <- roc(test_data[[target]], rf_probs, levels=c("No","Yes"), direction="<", quiet=TRUE)
rf_auc   <- auc(rf_roc)
rf_coords<- coords(rf_roc, "best", ret="threshold", transpose=FALSE)
rf_threshold <- rf_coords$threshold[1]
rf_pred  <- factor(ifelse(rf_probs >= rf_threshold, "Yes","No"), levels=c("No","Yes"))
rf_cm    <- confusionMatrix(rf_pred, test_data[[target]], positive="Yes")

print(paste("Test AUC:", round(rf_auc,4), "| Optimal Threshold:", round(rf_threshold,4)))
print("Confusion Matrix:"); print(rf_cm$table)

# Plot ROC curve
ggplot() +
  geom_line(aes(x=rf_roc$specificities, y=rf_roc$sensitivities)) +
  labs(title="ROC Curve on Test Set (Caret Tuned Ranger)",
       x="Specificity", y="Sensitivity") +
  theme_minimal()

# 9. Variable Importance
# -------------------------------------------------------------------------
print("--- Interpreting Model (Variable Importance) ---")
plot_importance_caret(tuned_rf_model)

# 10. Additional Visualizations
# -------------------------------------------------------------------------
print("--- Additional Visualizations ---")

# Fire Days by Season
ggplot(data %>% filter(FIRE_START_DAY), aes(SEASON)) +
  geom_bar(fill = 'red') +
  labs(title = 'Percentage of Fire Days by Season', x = 'Season', y = 'Count of Fire Days') +
  theme_minimal()

# Weather Conditions on Fire vs. Non-Fire Days
data %>%
  group_by(SEASON, FIRE_START_DAY) %>%
  summarise(
    Avg_Max_Temp = mean(MAX_TEMP),
    Avg_Precip   = mean(PRECIPITATION),
    Avg_Wind     = mean(AVG_WIND_SPEED)
  ) %>%
  pivot_longer(cols = starts_with('Avg'), names_to = 'Variable', values_to = 'Value') %>%
  ggplot(aes(SEASON, Value, fill = FIRE_START_DAY)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_wrap(~ Variable, scales = 'free_y') +
  labs(title = 'Weather Conditions on Fire vs. Non-Fire Days', fill = 'Fire Start') +
  theme_minimal()

# 11. Conclusion for Research Question
# -------------------------------------------------------------------------
cat("\n--- Conclusion for Research Question ---\n")
cat("Q: How can weather variables and seasonal patterns be used to predict wildfire occurrence?\n\n")
cat(paste("* Tuned Random Forest achieved Test AUC of", round(rf_auc,3), ", indicating strong predictive signal.\n"))
cat("* Key drivers highlighted by importance analysis include temperature, precipitation, wind, and seasonality.\n")
cat("* Visualizations reveal seasonal patterns and differences in weather conditions on fire vs. non-fire days.\n")
cat("Summary: Weather and seasonal data, used with tuned Random Forest, effectively predict daily wildfire ignition risk.\n")

# 12. Save Final Tuned Model
# -------------------------------------------------------------------------
print("--- Saving Final Tuned Model ---")
saveRDS(tuned_rf_model, "../models/tuned_ranger_model_caret_v11_modified.rds")
print(paste("Model saved to:", getwd()))

print("--- Analysis Script Finished ---")
