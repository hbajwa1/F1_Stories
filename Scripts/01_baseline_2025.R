library(f1dataR)
library(tidyverse)

# 1. Load 2025 Constructor Standings
# This tells the story of who dominated the 'Ground Effect' era
standings_2025 <- load_standings(season = 2025) %>%
  pluck("constructor_standings") # Extract the relevant table

# 2. Create baseline visualization of manufacturer standings
baseline_plot <- ggplot(standings_2025, aes(x = reorder(constructor_name, points), y = points, fill = constructor_name)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "F1 2025: The Final Standings",
    subtitle = "The hierarchy before the 2026 Regulation Reset",
    x = "Constructor",
    y = "Total Points"
  ) +
  theme(legend.position = "none")

# 3. Save it to your output folder
ggsave("output/2025_baseline_points.png", baseline_plot, width = 8, height = 5)

print("Story 1: Baseline captured and saved to output folder!")
