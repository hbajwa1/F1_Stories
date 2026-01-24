library(f1dataR)
library(tidyverse)
library(ggtext)

# 1. Load 2025 Constructor Standings
standings_2025 <- load_standings(season = 2025, type = "constructor")

standings_2025 <- standings_2025 %>%
  mutate(constructor_name = if_else(constructor_id == "rb", "Racing Bulls", constructor_id),
         constructor_name = as.factor(str_to_title(str_replace_all(constructor_name,"_", " "))),
         points = as.numeric(points),
         wins = as.numeric(wins)
         )

# 2. Build the Team Info Table (Automated)
team_info <- standings_2025 %>%
  select(constructor_id, constructor_name) %>%
  rowwise() %>%
  mutate(
    # Dynamically fetch colors using Python's FastF1 package
    team_color = get_team_color(constructor_id, season = 2025),

    # Create HTML labels for the Y-axis
    logo_label = paste0(
      "<img src='https://raw.githubusercontent.com/f1db/f1db/main/data/constructors/",
      constructor_id, "/logo.svg' width='30'/><br>",
      "**", constructor_name, "**"
    )
  ) %>%
  ungroup()

standings_2025 <- left_join(standings_2025, team_info,
                            by = c("constructor_id", "constructor_name"))
# 3. Join and Visualize
standings_2025 %>%
  ggplot(aes(x = fct_reorder(logo_label, points), y = points, fill = constructor_name)) +
  geom_col(width = 0.7) + # Adding a tiny border for definition
  coord_flip() +
  theme_dark_f1(axis_marks = TRUE) +
  scale_fill_manual(values = setNames(team_info$team_color, team_info$constructor_name)) +
  labs(
    title = "F1 2025: THE FINAL STANDINGS",
    subtitle = "The hierarchy before the 2026 Regulation Reset",
    x = NULL,
    y = "Total Points"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(lineheight = 1.1, color = "white"),
    plot.title = element_text(face = "bold", size = 18, color = "#E10600"), # F1 Red title
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# 3. Save it to your output folder
ggsave("output/2025_baseline_points.png", baseline_plot, width = 8, height = 5)

