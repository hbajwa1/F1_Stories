library(f1dataR)
library(tidyverse)
library(ggtext)
library(glue)

# Load helper functions
source("R/f1_helpers.R")

# -- PARAMETERS --
target_season <- 2025

# 1. Load constructor standings
constructor_standings <- load_standings(season = target_season, type = "constructor")

# 2. Load team colors
constructor_standings <- constructor_standings %>%
  rowwise() %>%
  mutate(
    constructor_name = translate_constructor(constructor_id, target_season, "clean_name"),
    python_id = translate_constructor(constructor_id, target_season, "fastf1_id"),
    team_color = get_team_color(python_id, season = target_season),
    points = as.numeric(points)
  ) %>%
  ungroup()

# 3. Baseline results of manufacturer standings
constructor_points <- constructor_standings %>%
  ggplot(aes(x = fct_reorder(constructor_name, points), y = points)) +

  geom_col(aes(fill = team_color), width = 0.6, color = "white", size = 0.1) +
  coord_flip() +

  scale_fill_identity() +
  theme_dark_f1(axis_marks = TRUE) +

  labs(
    title = glue("F1 {target_season}: CONSTRUCTOR STANDINGS"),
    subtitle = "Performance baseline prior to the 2026 Regulations",
    x = NULL,
    y = "Total Championship Points"
  )

# 3. Save it to your output folder
ggsave(glue("output/constructor_standings/season_{target_season}.png"), constructor_points)

# Overwrite the 'latest' version for the README
ggsave("output/latest_standings.png", constructor_points)

