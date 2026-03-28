library(tidyverse)
library(f1dataR)
library(glue)
library(ggtext)

source("R/f1_helpers.R")

# --- 1. BUILD THE HISTORICAL MASTER FILE ---

if (!dir.exists("data/raw_seasons")) dir.create("data/raw_seasons", recursive = TRUE)

years_to_analyze <- 2015:2025

# 2. Wrap your processing function with a "Save" check
build_and_save_season <- function(year) {
  file_path <- paste0("data/raw_seasons/season_", year, ".rds")

  # SKIP if we already have it (saves API calls on repeat runs)
  if (file.exists(file_path)) {
    message(paste("Skipping", year, "- file already exists."))
    return(readRDS(file_path))
  }

  message(paste("Processing season:", year, "..."))

  # Your existing master logic
  season_data <- build_master_season_data(year)

  # Save the individual year immediately
  saveRDS(season_data, file_path)

  # IMPORTANT: Add a small sleep to avoid hitting the rate limit
  Sys.sleep(2)

  return(season_data)
}

# 3. Run the loop safely
master_history <- map_dfr(years_to_analyze, build_and_save_season)

master_history <- master_history %>%
  mutate(points = as.numeric(points),
         grid = as.numeric(grid),
         position = as.numeric(position),
         fastest_rank = as.numeric(fastest_rank),
         top_speed_kph = as.numeric(top_speed_kph)
         )

# --- 2. BUILD SUMMMARY DATA BY CONSTRUCTOR ---

constructor_season <- master_history %>%
  group_by(constructor_id, season) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  arrange(season, desc(total_points)) %>%
  group_by(season) %>%
  mutate(constructor_ranking = row_number()) %>%
  ungroup() %>%
  group_by(constructor_id) %>%
  mutate(mean_constructor_ranking = round(mean(constructor_ranking), digits = 1)) %>%
  ungroup()

constructor_track <- master_history %>%
  group_by(constructor_id, circuit_id) %>%
  summarise(total_points = sum(points, na.rm = TRUE),
            mean_position = round(mean(position), digits = 2),
            .groups = "drop",) %>%
  arrange(constructor_id, mean_position)


