#::::::::::::::::::::::::::::::::::::::::::::#
#           Helper functions
#::::::::::::::::::::::::::::::::::::::::::::#

#################################################################

#' Build a Master Data Frame for an F1 Season
#' Joins Schedule, Race Results (for constructor_id), and Qualifying Times
build_master_season_data <- function(season_year) {
  require(f1dataR)
  require(tidyverse)

  message(glue::glue("🔨 Building Master Data for {season_year}..."))

  # 1. Load Schedule (Track Info)
  sched <- load_schedule(season = season_year) %>%
    mutate(season = as.numeric(season_year),
           round = as.numeric(round)) %>%
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
        mutate(best_quali_time = min(c(q1_sec, q2_sec, q3_sec), na.rm = TRUE)) %>%
        ungroup() %>%
        select(driver_id, best_quali_time)

      # Merge Results and Quali on driver_id
      round_data <- results %>%
        left_join(quali, by = "driver_id",
                  relationship = "one-to-one") %>%
        mutate(season = season_year, round = r)

      return(round_data)
    }, error = function(e) return(NULL))
  })

  # 3. Final Join with Schedule (Circuit Name, Date, etc.)
  final_master <- master_df %>%
    left_join(sched, by = c("season", "round"),
              relationship = "many-to-one")

  return(final_master)
}

#################################################################

#' Efficient Constructor Translation
#' Apply the mapping to a whole dataframe at once
apply_constructor_mapping <- function(data) {
  # 1. Load the mapping file once
  mapping <- readr::read_csv("data/manufacturer_driver_translation_table.csv", show_col_types = FALSE)

  # 2. Join the mapping to your data
  # This replaces the need for a loop/rowwise function
  data %>%
    left_join(mapping, by = c("constructor_id"),
              relationship = "many-to-one")
}

