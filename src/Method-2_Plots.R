# === Install & Load Required Libraries ===

library(ggplot2)
library(dplyr)
library(scales)

# === Step 1: Read the Cause 14 Prediction File ===
df14 <- read.csv("../data/cause14_binary_predictions_with_confidence.csv", stringsAsFactors = FALSE)

# === Step 2: Create Confidence Category Labels ===
df14$Confidence_Level <- dplyr::case_when(
  df14$Predicted_Category == "Human-made" & df14$Prob_Human >= 0.8 ~ "Human-made (Confident)",
  df14$Predicted_Category == "Natural" & df14$Prob_Human <= 0.2 ~ "Natural (Confident)",
  TRUE ~ "Needs Review"
)

# === PART 9: Pie Chart of Confidence Category Breakdown ===
df_conf <- df14 %>%
  count(Confidence_Level) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(Confidence_Level, "\n", n, " (", Percent, "%)"))

ggplot(df_conf, aes(x = "", y = n, fill = Confidence_Level)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4.5) +
  scale_fill_manual(values = c("Human-made (Confident)" = "red",
                               "Natural (Confident)" = "blue",
                               "Needs Review" = "gray")) +
  labs(title = "Cause 14: Confidence Category Breakdown", fill = "Prediction Certainty") +
  theme_void()

# === Step 3: Extract Month from ALARM_DATE ===
df14$month <- as.numeric(format(as.Date(df14$ALARM_DATE), "%m"))

# === PART 10: Monthly Bar Chart of Predicted Categories ===
df_monthly <- df14 %>%
  group_by(month, Predicted_Category) %>%
  summarise(FireCount = n(), .groups = "drop")

ggplot(df_monthly, aes(x = factor(month), y = FireCount, fill = Predicted_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = FireCount), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Human-made" = "red", "Natural" = "blue")) +
  labs(title = "Predicted Cause 14 Fires by Month",
       x = "Month", y = "Number of Fires", fill = "Predicted Category") +
  theme_minimal()
