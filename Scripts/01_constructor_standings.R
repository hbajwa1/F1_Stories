library(tidyverse)
library(f1dataR)
library(glue)
library(ggtext)

source("R/f1_helpers.R")

# --- 1. BUILD THE HISTORICAL MASTER FILE ---
years_to_analyze <- c(2022, 2023, 2024, 2025)
master_history <- map_dfr(years_to_analyze, build_master_season_data)

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

  # Step (d): Average gap from pole for each team
  group_by(season, constructor_id) %>%
  summarise(
    team_avg_quali_gap = mean(gap_from_pole),
    .groups = "drop"
  )



