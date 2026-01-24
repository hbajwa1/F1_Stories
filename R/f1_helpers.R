# R/f1_helpers.R

#' Translate Constructor IDs and Names
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
