# Extracting F1 team logos
library(magick)
library(glue)

#' Fetch and Cache F1 Team Logo
#'
#' @param constructor_id The F1 constructor ID (e.g., "ferrari", "red_bull")
#' @param size Width in pixels for the PNG (default 100)
#' @return A local file path to the PNG
get_cached_logo <- function(constructor_id, size = 100) {

  # 1. Setup Directory Structure for Shiny (www/logos)
  # We use 'www' because Shiny serves static files from here automatically
  logo_dir <- "www/logos"
  if (!dir.exists(logo_dir)) dir.create(logo_dir, recursive = TRUE)

  # 2. Define Paths
  # The GitHub source for high-quality SVGs
  remote_url <- glue("https://raw.githubusercontent.com/f1db/f1db/main/data/constructors/{constructor_id}/logo.svg")
  local_path <- file.path(logo_dir, glue("{constructor_id}.png"))

  # 3. Check Cache (If we have it, don't download it again)
  if (file.exists(local_path)) {
    return(local_path)
  }

  # 4. Download and Convert (Error Handling included)
  tryCatch({
    # Read SVG from URL
    img <- image_read_svg(remote_url, height = size) # Load height to maintain aspect ratio

    # Write as PNG (Better for R plotting engines than SVG)
    image_write(img, path = local_path, format = "png")

    return(local_path)

  }, error = function(e) {
    message(glue("Warning: Could not fetch logo for {constructor_id}. Using placeholder."))
    return(NA) # Return NA so we can filter it out or handle it gracefully
  })
}
