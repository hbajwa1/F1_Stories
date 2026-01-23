library(f1dataR)
library(tidyverse)
library(ggtext)

# 1. Load 2025 Constructor Standings
standings_2025 <- load_standings(season = 2025, type = "constructor")

standings_2025 <- standings_2025 %>%
  mutate(constructor_id = factor(constructor_id),
         points = as.numeric(points))

# 2. Get Team Colors & Logo URLs
team_info <- standings_2025 %>%
  distinct(constructor_id) %>%
  mutate(
    team_color = map_chr(constructor_id, ~get_team_color(.x, season = 2025)),
    # Create an HTML string for the logo. Adjust 'width' as needed.
    logo_label = paste0("<img src='",
                        # We use the team's official ID to find logos (usually on Ergast/F1 servers)
                        "https://raw.githubusercontent.com/f1db/f1db/main/data/constructors/",
                        constructor_id, "/logo.svg' width='25'/><br>",
                        constructor_id)
  )

# 2. Create baseline visualization of manufacturer standings
base_line_plot <- standings_2025 %>%
  ggplot(aes(x = fct_reorder(constructor_id, points), y = points)) +
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
