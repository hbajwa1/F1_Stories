library(tidyverse)
library(f1dataR)
library(glue)
library(ggtext)

source("R/f1_helpers.R")

# --- 1. BUILD THE HISTORICAL MASTER FILE ---
years_to_analyze <- c(2022, 2023, 2024, 2025)
start_yr <- min(years_to_analyze)
end_yr <- max(years_to_analyze)

file_name <- glue::glue("f1_results_{start_yr}_{end_yr}.csv")
file_path <- file.path("data", file_name)

if (file.exists(file_path)) {

  message(glue::glue("✅ Local cache found: {file_name}. Reading file..."))
  raw_history <- read_csv(file_path, show_col_types = FALSE)

} else {

  message("🌐 No local cache found. Fetching data from Ergast API (this may take a minute)...")

  # Run master data builder function
  raw_history <- map_dfr(years_to_analyze, build_master_season_data)

  if (!dir.exists("data")) dir.create("data") # Ensure directory exists
  write_csv(raw_history, file_path)
  message(glue::glue("💾 Data saved to {file_path}"))

}

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

  filter(!is.infinite(team_best_quali)) %>%

  # Step (c): Identify percentage gap of each team from best qualifying time
  mutate(gap_from_pole = ((team_best_quali/pole_time) - 1)) %>%

  # Step (d): Clean manufacturer names using translation table
  apply_constructor_mapping() %>%

  # Step (d): Apply weights to make more recent performance account for more
  mutate(year_weights = case_when(
    season == 2022 ~ 0.1,
    season == 2023 ~ 0.2,
    season == 2024 ~ 0.3,
    season == 2025 ~ 0.4
    )) %>%

  # Step (e): Calculate weighted rankings of constructors (2022-2025)
  group_by(clean_name) %>%
  summarise(
    team_avg_gap_pole = sum(gap_from_pole * year_weights, na.rm = TRUE) / sum(year_weights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(team_avg_gap_pole)

# --- 3. VISUALIZE MANUFACTURING RANKINGS ---

tech_index %>%
  mutate(ranking_diff = (team_avg_gap_pole - team_avg_gap_pole[1])*100) %>%

  ggplot(aes(x = reorder(clean_name, -ranking_diff), y =ranking_diff)) +
  geom_col() +
  coord_flip() +
  ylab("Average relative gap to leader (%)") + xlab("") +
  ggtitle("F1 Manufacturer Performance (2022-2025)")






