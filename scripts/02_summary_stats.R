source("R/f1_helpers.R")

# --- 1. BUILD SUMMMARY DATA BY CONSTRUCTOR ---

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

# --- 2. BUILD SUMMMARY DATA BY DRIVER ---

driver_season <- master_history %>%
  group_by(driver_id, season) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  arrange(season, desc(total_points)) %>%
  group_by(season) %>%
  mutate(driver_ranking = row_number()) %>%
  ungroup() %>%
  group_by(driver_id) %>%
  mutate(mean_driver_ranking = round(mean(driver_ranking), digits = 1)) %>%
  ungroup()

driver_track <- master_history %>%
  group_by(driver_id, circuit_id) %>%
  summarise(total_points = sum(points, na.rm = TRUE),
            mean_position = round(mean(position), digits = 2),
            .groups = "drop",) %>%
  arrange(driver_id, mean_position)
