library(dplyr)
library(caret)
library(randomForest)
library(xgboost)

# Step 1: Load the dataset
fires <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)

# Line-11 till Line-94 is code to train the model. It is commented out for simplicity and to save time. The models trained in the same folder.

# # Step 2: Add month as a numeric feature
# fires$month <- as.numeric(format(as.Date(fires$ALARM_DATE), "%m"))
# ## 5-FOLD RF MODEL##
# # Step 3: Filter only known causes 1 to 13 (exclude cause 14)
# fires_known <- fires %>%
#   filter(CAUSE %in% 1:13)
# 
# # Step 4: Check how many records per cause before removing NA
# cat("Before NA handling:\n")
# print(table(fires_known$CAUSE))
# 
# # Step 5: Remove rows with NA only for selected predictor columns
# predictors <- c("FIRE_DURATION", "Temperature_C", "MaxTemperature_C",
#                 "WindSpeed_kmh", "MaxWindSpeed_kmh", "Shape__Area", "month")
# fires_known <- fires_known %>%
#   filter(complete.cases(select(., all_of(predictors))))
# 
# # Step 6: Check if all 13 causes are still represented
# cat("After NA handling:\n")
# print(table(fires_known$CAUSE))
# 
# # Step 7: Convert CAUSE to factor
# fires_known$CAUSE <- as.factor(fires_known$CAUSE)
# 
# # Step 8: Set up 5-fold cross-validation with upsampling for rare classes
# set.seed(123)
# train_control <- trainControl(method = "cv", number = 5, sampling = "up")
# 
# # Step 9: Train Random Forest model with cross-validation
# model_cv <- train(CAUSE ~ FIRE_DURATION + Temperature_C + MaxTemperature_C +
#                     WindSpeed_kmh + MaxWindSpeed_kmh + Shape__Area + month,
#                   data = fires_known,
#                   method = "rf",
#                   trControl = train_control,
#                   ntree = 200,
#                   tuneLength = 6)  # Try 3 values for mtry
# 
# # Step 10: View results
# print(model_cv)
# cat("Cross-validated Accuracy:", round(max(model_cv$results$Accuracy) * 100, 2), "%\n")
# 
# # Step 11: Feature Importance
# importance_plot <- varImp(model_cv)
# print(importance_plot)
# 
# # Optional: Plot accuracy vs mtry
# plot(model_cv)
# 
# saveRDS(rf_model, "5_fold_rf.rds")
# 
# 
# ## 5-FOLD XG-BOOST MODEL##
# # Prepare features and filter only known causes (1–13)
# fires$month <- as.numeric(format(as.Date(fires$ALARM_DATE), "%m"))
# 
# fires_known <- fires %>%
#   filter(CAUSE %in% 1:13) %>%
#   select(CAUSE, FIRE_DURATION, Temperature_C, MaxTemperature_C,
#          WindSpeed_kmh, MaxWindSpeed_kmh, Shape__Area, month) %>%
#   filter(complete.cases(.))  # Handle missing values
# 
# fires_known$CAUSE <- as.factor(fires_known$CAUSE)
# 
# # ️ Set up cross-validation
# set.seed(123)
# cv_control <- trainControl(method = "cv", number = 5)
# 
# # Train XGBoost model
# xgb_model <- train(CAUSE ~ FIRE_DURATION + Temperature_C + MaxTemperature_C +
#                      WindSpeed_kmh + MaxWindSpeed_kmh + Shape__Area + month,
#                    data = fires_known,
#                    method = "xgbTree",
#                    trControl = cv_control,
#                    tuneLength = 5)
# 
# #  Print results
# #print(xgb_model)
# cat("cross-validated Accuracy (XGBoost):", round(max(xgb_model$results$Accuracy) * 100, 2), "%\n")
# 
# # Feature Importance
# #importance_plot <- varImp(xgb_model)
# #print(importance_plot)
# 
# saveRDS(xgb_model, "xgb_model.rds")


#  Load trained models
rf_model <- readRDS("../models/5_fold_rf.rds")
xgb_model <- readRDS("../models/xgb_model.rds")

# Filter only Cause 14 records and prepare features
cause14 <- fires %>%
  filter(CAUSE == 14) %>%
  mutate(month = as.numeric(format(as.Date(ALARM_DATE), "%m"))) %>%
  select(FIRE_DURATION, Temperature_C, MaxTemperature_C,
         WindSpeed_kmh, MaxWindSpeed_kmh, Shape__Area, month) %>%
  na.omit()

# Keep a copy of original rows for final output
cause14_original <- fires %>%
  filter(CAUSE == 14) %>%
  filter(complete.cases(select(., FIRE_DURATION, Temperature_C, MaxTemperature_C,
                               WindSpeed_kmh, MaxWindSpeed_kmh, Shape__Area))) %>%
  mutate(month = as.numeric(format(as.Date(ALARM_DATE), "%m")))

#  Predict with Random Forest
rf_probs <- predict(rf_model, newdata = cause14, type = "prob")
rf_pred <- colnames(rf_probs)[apply(rf_probs, 1, which.max)]
rf_conf <- apply(rf_probs, 1, max)

# Predict with XGBoost
xgb_probs <- predict(xgb_model, newdata = cause14, type = "prob")
xgb_pred <- colnames(xgb_probs)[apply(xgb_probs, 1, which.max)]
xgb_conf <- apply(xgb_probs, 1, max)

#  Attach predictions to cause14_original
cause14_original$RF_Predicted_Cause <- rf_pred
cause14_original$RF_Confidence <- rf_conf
cause14_original$XGB_Predicted_Cause <- xgb_pred
cause14_original$XGB_Confidence <- xgb_conf
cause14_original$Match <- ifelse(rf_pred == xgb_pred, "Yes", "No")

#  Save to CSV
write.csv(cause14_original, "../data/cause14_dual_model_predictions_5fold.csv", row.names = FALSE)
#cat("Predictions saved to 'cause14_dual_model_predictions_5fold.csv'\n")


# Load data
df <- read.csv("../data/cause14_dual_model_predictions_5fold.csv", stringsAsFactors = FALSE)

# Define ensemble-based decision rule
df$Final_Classification <- ifelse(
  df$Match == "Yes", 
  df$RF_Predicted_Cause,  # Agreement → take any
  ifelse(
    df$RF_Confidence >= 0.8 & df$RF_Confidence > df$XGB_Confidence,
    df$RF_Predicted_Cause,
    ifelse(
      df$XGB_Confidence >= 0.8 & df$XGB_Confidence > df$RF_Confidence,
      df$XGB_Predicted_Cause,
      "Needs Review"
    )
  )
)

# See how many were confidently classified
#table(df$Final_Classification)

# Save new classification to a CSV
write.csv(df, "../data/cause14_final_ensemble_classification.csv", row.names = FALSE)
# cat("Saved to 'cause14_final_ensemble_classification.csv'\n")


