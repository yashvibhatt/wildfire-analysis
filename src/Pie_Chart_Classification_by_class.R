library(ggplot2)
library(dplyr)

df <- read.csv("../data/fires_with_max_weather_corrected.csv", stringsAsFactors = FALSE)
df <- df[!is.na(df$CAUSE), ]
df$CAUSE <- as.integer(df$CAUSE)

# Define labels
cause_labels <- c(
  "1" = "Lightning", "2" = "Equipment Use", "3" = "Smoking", "4" = "Campfire",
  "5" = "Debris", "6" = "Railroad", "7" = "Arson", "8" = "Playing with Fire",
  "9" = "Miscellaneous", "10" = "Vehicle", "11" = "Powerline",
  "12" = "Firefighter Training", "13" = "Non-Firefighter Training",
  "14" = "Unknown", "15" = "Structure", "16" = "Aircraft", "17" = "Volcanic",
  "18" = "Prescribed Burn", "19" = "Illegal Campfire"
)

# Prepare and group
cause_dist <- df %>%
  count(CAUSE) %>%
  mutate(
    Label = cause_labels[as.character(CAUSE)],
    Percent = n / sum(n),
    Group = ifelse(Percent < 0.03, "Other Causes", Label)
  ) %>%
  group_by(Group) %>%
  summarise(Total = sum(n)) %>%
  mutate(
    Fraction = Total / sum(Total),
    ymax = cumsum(Fraction),
    ymin = lag(ymax, default = 0),
    LabelPos = (ymin + ymax) / 2,
    Angle = 360 * LabelPos,
    LabelText = paste0(Group, "\n", Total, " (", round(Fraction * 100, 1), "%)")
  )

# Plot donut with visible labels
ggplot(cause_dist, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = Group)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0, 5.5)) +  # Increase for label space
  geom_text(
    aes(x = 5.2, y = LabelPos, label = LabelText, angle = 90 - Angle),
    hjust = 0, size = 3.5, color = "black", fontface = "bold"
  ) +
  scale_fill_manual(values = colorRampPalette(c("firebrick", "orange", "forestgreen", "steelblue", "purple"))(nrow(cause_dist))) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Distribution of Fire Causes (with Angled Labels)")
