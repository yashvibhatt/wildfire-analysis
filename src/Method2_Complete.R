tinytex::install_tinytex()

# Load packages
library(e1071)
library(caret)
library(dplyr)

# Load and prepare data
df <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)

# Recode fire cause
human_causes <- c(2:13, 15:16, 18:19)
natural_causes <- c(1, 17)

df <- df %>%
  filter(CAUSE %in% c(human_causes, natural_causes)) %>%
  mutate(CAUSE_BINARY = ifelse(CAUSE %in% human_causes, 1, 0),
         CAUSE_BINARY = as.factor(CAUSE_BINARY)) %>%
  select(CAUSE_BINARY, FIRE_DURATION, Temperature_C, MaxTemperature_C,
         WindSpeed_kmh, MaxWindSpeed_kmh, Shape__Area, ALARM_DATE) %>%
  filter(complete.cases(.))

df$month <- as.numeric(format(as.Date(df$ALARM_DATE), "%m"))

# Create feature matrix and labels
X <- df[, c("FIRE_DURATION", "Temperature_C", "MaxTemperature_C",
            "WindSpeed_kmh", "MaxWindSpeed_kmh", "Shape__Area", "month")]
y <- df$CAUSE_BINARY

# Try different weight ratios
weight_ratios <- c(1, 1.25, 1.5, 1.75, 2)
results <- list()

for (w in weight_ratios) {
  cat("\nTraining SVM with weight ratio 0:1 =", w, ":1\n")
  
  # Class weights: give more weight to minority class (0)
  model <- svm(x = X, y = y,
               kernel = "radial",
               cost = 1,
               gamma = 0.1,
               class.weights = c("0" = w, "1" = 1),
               scale = TRUE,
               probability = FALSE)
  
  preds <- predict(model, X)
  cm <- confusionMatrix(preds, y)
  
  cat("Accuracy:", round(cm$overall["Accuracy"], 4), "\n")
  cat("Sensitivity (Natural):", round(cm$byClass["Sensitivity"], 4), "\n")
  cat("Specificity (Human-made):", round(cm$byClass["Specificity"], 4), "\n")
  
  results[[as.character(w)]] <- cm
}

# Train final model with 1.5:1 weight
best_model <- svm(x = X, y = y,
                  kernel = "radial",
                  cost = 1,
                  gamma = 0.1,
                  class.weights = c("0" = 1.5, "1" = 1),
                  scale = TRUE,
                  probability = TRUE)  # enable probabilities if needed later

# Save the model
saveRDS(best_model, "../models/svm_model_weight1.5.rds")

# Extract metrics
weights <- c(1, 1.25, 1.5, 1.75, 2)
acc <- sapply(results, function(x) x$overall["Accuracy"])
sens <- sapply(results, function(x) x$byClass["Sensitivity"])
spec <- sapply(results, function(x) x$byClass["Specificity"])

# Plot
library(ggplot2)
df_plot <- data.frame(Weight = weights, Accuracy = acc, Sensitivity = sens, Specificity = spec)

ggplot(df_plot, aes(x = Weight)) +
  geom_line(aes(y = Accuracy, color = "Accuracy"), size = 1.2) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity"), size = 1.2) +
  geom_line(aes(y = Specificity, color = "Specificity"), size = 1.2) +
  labs(title = "Model Performance vs. Class Weight for Natural Fires",
       x = "Weight Ratio (Class 0 : Class 1)",
       y = "Metric Value",
       color = "Metric") +
  theme_minimal()


# Predict with probabilities
pred_probs <- predict(best_model, newdata = X14, probability = TRUE)
probs <- attr(pred_probs, "probabilities")

# Attach predictions and probabilities to df14
df14$Predicted_Category <- ifelse(pred_probs == 0, "Natural", "Human-made")
df14$Prob_Natural <- probs[, "0"]
df14$Prob_Human <- probs[, "1"]

# Save to CSV for reporting
write.csv(df14, "../data/cause14_binary_predictions_with_confidence.csv", row.names = FALSE)
