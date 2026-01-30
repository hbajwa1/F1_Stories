library(tidyverse)
library(f1dataR)
library(glue)
library(ggtext)

source("R/f1_helpers.R")

# --- 1. BUILD THE HISTORICAL MASTER FILE ---
years_to_analyze <- c(2022, 2023, 2024, 2025)
master_history <- map_dfr(years_to_analyze, build_master_season_data)

write_csv(master_history, "data/f1_results.csv")

# --- 2. CALCULATE THE TECHNICAL INDEX ---
tech_index <- master_history %>%

  # Step (a): Identify pole time in each qualifying
  group_by(season, round) %>%
  mutate(pole_time = min(best_quali_time, na.rm = TRUE)) %>%

  # Step (b): Identify best qualifying team by team
  group_by(season, round, constructor_id) %>%
  summarise(
    team_best_quali = min(best_quali_time, na.rm = TRUE),
    pole_time = first(pole_time),
    .groups = "drop"
  ) %>%

  # Step (c): Identify percentage gap of each team from best qualifying time
  mutate(gap_from_pole = ((team_best_quali/pole_time) - 1)) %>%

  # Step (d): Apply weights to make more recent performance account for more
  mutate(year_weights = case_when(
    season == 2022 ~ 0.1,
    season == 2023 ~ 0.2,
    season == 2024 ~ 0.3,
    season == 2025 ~ 0.4
    )) %>%

  # Step (e): Calculate weighted rankings of constructors (2022-2025)
  group_by(constructor_id) %>%
  summarise(
    team_avg_gap_pole = sum(gap_from_pole * year_weights, na.rm = TRUE) / sum(year_weights, na.rm = TRUE)
  ) %>%
  arrange(team_avg_gap_pole)









