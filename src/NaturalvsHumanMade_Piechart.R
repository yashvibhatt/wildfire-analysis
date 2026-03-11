library(ggplot2)
library(dplyr)
library(scales)

# Load your full dataset again
df_all <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)

# Define cause groups
human_causes <- c(2:13, 15:16, 18:19)
natural_causes <- c(1, 17)

df_labeled <- df_all %>%
  filter(CAUSE %in% c(human_causes, natural_causes)) %>%
  mutate(Category = ifelse(CAUSE %in% human_causes, "Human-made", "Natural"))

# Count and percentage
df_pie <- df_labeled %>%
  count(Category) %>%
  mutate(Percentage = round(100 * n / sum(n), 1),
         Label = paste0(Category, "\n", n, " (", Percentage, "%)"))

# Plot
ggplot(df_pie, aes(x = "", y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Human-made" = "darkred", "Natural" = "navyblue")) +
  labs(title = "Fire Cause Breakdown (Labeled Data)", fill = "Cause Type") +
  theme_void()

