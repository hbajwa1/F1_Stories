library(shiny)
library(bslib)
library(tidyverse)
library(bsicons)

# --- 1. IMPORT DATA ---
# In production, you will load "data/dashboard_ready.rds" here
drivers_list <- c("Max Verstappen", "Lewis Hamilton", "Charles Leclerc")
teams_list <- c("Red Bull", "Mercedes", "Ferrari", "McLaren")
tracks_list <- c("Global Summary" = "all", "Silverstone" = "silverstone", "Monaco" = "monaco")

# --- 2. USER INTERFACE ---
ui <- page_fillable(
  theme = bs_theme(bootswatch = "darkly"),

  # Title / Header
  div(class = "d-flex justify-content-between align-items-center mb-3",
      h2("🏎️ F1 Performance Dashboard"),
      span("Data: 2022-2025 Ground Effect Era", class = "text-muted")
  ),

  # THE MAIN GRID LAYOUT (Matches your Sketch)
  layout_columns(
    col_widths = c(8, 4), # Left column wider (Graph + Map), Right column narrower (Controls + Stats)
    row_heights = c(1, 2), # Top row shorter, bottom row taller

    # --- QUADRANT 1 (Top Left): TRACK CONTEXT ---
    card(
      card_header("Track Context"),
      layout_sidebar(
        sidebar = sidebar(
          open = "always", width = "40%",
          selectInput("track_select", "Select Circuit:", choices = tracks_list, selected = "all"),
          helpText("Select 'Global Summary' to see career stats, or a specific track to drill down.")
        ),
        # The Track Map acts as the visual anchor here
        card_body(
          plotOutput("track_map_plot", height = "100%")
        )
      )
    ),

    # --- QUADRANT 2 (Top Right): SUBJECT SELECTION ---
    card(
      card_header("Subject Selection"),
      # The "Mode Switch" (Driver vs Team)
      radioButtons("analysis_mode", NULL,
                   choices = c("Driver", "Manufacturer"),
                   selected = "Driver",
                   inline = TRUE),
      # The Dynamic Dropdown (Updates based on mode)
      selectInput("subject_select", "Select Name:", choices = drivers_list)
    ),

    # --- QUADRANT 3 (Bottom Left): RANKING GRAPH ---
    card(
      full_screen = TRUE,
      card_header(textOutput("graph_title")), # Dynamic Title
      plotOutput("ranking_plot", height = "100%")
    ),

    # --- QUADRANT 4 (Bottom Right): SUMMARY STATS ---
    card(
      card_header("Performance Profile"),
      # Using value_box for big numbers (Wins, Championships)
      layout_column_wrap(
        width = 1,
        value_box(
          title = "Total Wins",
          value = textOutput("stat_wins"),
          showcase = bs_icon("trophy-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Avg Finish",
          value = textOutput("stat_avg_finish"),
          showcase = bs_icon("flag-fill"),
          theme = "secondary"
        ),
        value_box(
          title = "Seasons Active",
          value = textOutput("stat_seasons"),
          showcase = bs_icon("calendar-event")
        )
      )
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {

  # A. DYNAMIC DROPDOWN LOGIC
  # When user toggles "Manufacturer" vs "Driver", update the list
  observeEvent(input$analysis_mode, {
    if (input$analysis_mode == "Driver") {
      updateSelectInput(session, "subject_select", choices = drivers_list)
    } else {
      updateSelectInput(session, "subject_select", choices = teams_list)
    }
  })

  # B. PLOT TITLE LOGIC
  output$graph_title <- renderText({
    if (input$track_select == "all") {
      return(paste("Yearly Championship Ranking:", input$subject_select))
    } else {
      return(paste("Race Results at", tools::toTitleCase(input$track_select), "-", input$subject_select))
    }
  })

  # C. RANKING GRAPH LOGIC (The Core "Normalized" Graph)
  output$ranking_plot <- renderPlot({
    # 1. Create Dummy Data for visualization
    years <- 2022:2025

    # Logic: Is it Global or Track specific?
    if (input$track_select == "all") {
      # GLOBAL MODE: Plot Championship Rank (1st, 2nd, etc.)
      # In real app: Filter data by input$subject_select
      y_values <- sample(1:5, 4, replace = TRUE)
      y_label <- "Championship Position"
    } else {
      # TRACK MODE: Plot Race Finish Position
      y_values <- sample(1:20, 4, replace = TRUE)
      y_label <- "Race Finishing Position"
    }

    df <- data.frame(Year = years, Rank = y_values)

    # 2. Draw Plot
    ggplot(df, aes(x = Year, y = Rank)) +
      geom_line(color = "#E10600", linewidth = 1.2) + # F1 Red
      geom_point(color = "white", size = 3) +
      scale_y_reverse(breaks = 1:20) + # Rank 1 is at the top!
      theme_dark() +
      theme(
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white")
      ) +
      labs(y = y_label)
  })

  # D. SUMMARY STATS LOGIC
  output$stat_wins <- renderText({
    # Real Logic: Filter data by Subject AND Track
    # If Track == "All", sum(wins). If Track == "Monaco", sum(wins at Monaco)
    if (input$track_select == "all") return("103") # Mock Total
    return("8") # Mock Track specific
  })

  output$stat_avg_finish <- renderText({
    if (input$track_select == "all") return("3.5")
    return("2.1")
  })

  output$stat_seasons <- renderText({
    "12"
  })

  # E. TRACK MAP LOGIC
  output$track_map_plot <- renderPlot({
    if (input$track_select == "all") {
      # If Global, maybe show a generic world map or logo
      ggplot() +
        geom_text(aes(x=0.5, y=0.5, label = "GLOBAL SUMMARY\n(Select a track to filter)"), color="white") +
        theme_void()
    } else {
      # If Track selected, show the specific circuit shape
      ggplot() +
        geom_text(aes(x=0.5, y=0.5, label = paste("Map of", input$track_select)), color="white", size=6) +
        theme_void() +
        theme(panel.border = element_rect(color = "white", fill = NA))
    }
  })
}

shinyApp(ui, server)
