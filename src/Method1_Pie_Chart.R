# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Read the predictions CSV
df <- read_csv("../data/cause14_final_ensemble_classification.csv")

# Prepare summary table
df_summary <- df %>%
  mutate(Classification = ifelse(Match == "Yes", "Classified (Match)", "Unmatched (Disagree)")) %>%
  count(Classification) %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(Classification, "\n", n, " (", round(perc, 1), "%)"))

# Plot donut chart
ggplot(df_summary, aes(x = 2, y = n, fill = Classification)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Classified (Match)" = "#4CAF50", 
                               "Unmatched (Disagree)" = "#F44336")) +
  ggtitle("Classification Outcome on Cause 14 Fires (Ensemble Models)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")