# R/f1_helpers.R

#' Translation tables for Constructors and Drivers
#'
#' @param id The ID from Ergast/f1dataR
#' @param season The year of the competition
#' @param target_type Either 'fastf1' or 'clean_name'
translate_constructor <- function(id, season, target_type = "clean_name") {
  # Load the mapping file
  mapping <- readr::read_csv("data/constructor_mapping.csv", show_col_types = FALSE)

  # Filter for the specific season and ID
  match <- mapping %>%
    filter(season == !!season, ergast_id == !!id)

  if (nrow(match) > 0) {
    return(match[[target_type]])
  } else {
    # If no match found, just clean up the ID (fallback)
    return(stringr::str_to_title(gsub("_", " ", id)))
  }
}

#################################################################

#' Build a Master Data Frame for an F1 Season
#' Joins Schedule, Race Results (for constructor_id), and Qualifying Times
build_master_season_data <- function(season_year) {
  require(f1dataR)
  require(tidyverse)

  message(glue::glue("🔨 Building Master Data for {season_year}..."))

  # 1. Load Schedule (Track Info)
  sched <- load_schedule(season = season_year) %>%
    mutate(season = as.numeric(season_year)) %>%
    # Select only the rounds that have already occurred
    filter(as.Date(date) < Sys.Date())

  # 2. Loop through rounds to get Results & Quali
  master_df <- map_dfr(sched$round, function(r) {
    tryCatch({
      # Load Race Results (to get the Driver <-> Constructor link)
      results <- load_results(season = season_year, round = r) %>%
        select(driver_id, constructor_id, points, grid, position)

      # Load Qualifying Data
      quali <- load_quali(season = season_year, round = r) %>%
        rowwise() %>%
        # Calculate driver's theoretical best lap
        mutate(best_time_sec = min(c(q1_sec, q2_sec, q3_sec), na.rm = TRUE)) %>%
        ungroup() %>%
        select(driver_id, best_time_sec)

      # Merge Results and Quali on driver_id
      round_data <- results %>%
        left_join(quali, by = "driver_id") %>%
        mutate(season = season_year, round = r)

      return(round_data)
    }, error = function(e) return(NULL))
  })

  # 3. Final Join with Schedule (Circuit Name, Date, etc.)
  final_master <- master_df %>%
    left_join(sched, by = c("season", "round"))

  return(final_master)
}
