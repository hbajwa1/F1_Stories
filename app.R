library(shiny)
library(bslib)
library(tidyverse)
library(f1dataR)

# --- 1. GLOBAL SETUP (Data Loading) ---
# In the future, we will load your "master_history.rds" here.
# For now, I'll create dummy lists to populate the dropdowns.
all_drivers <- c("Max Verstappen", "Lewis Hamilton", "Fernando Alonso", "Lando Norris")
all_teams <- c("Red Bull", "Mercedes", "Ferrari", "McLaren", "Aston Martin")
all_circuits <- c("Bahrain", "Monaco", "Silverstone", "Spa-Francorchamps")

# --- 2. USER INTERFACE (The Sketch) ---
ui <- page_sidebar(
  title = "🏎️ F1 Stories Dashboard",
  theme = bs_theme(bootswatch = "darkly"), # Matches your dark theme aesthetic

  # A. The Sidebar Menu (Left in your sketch)
  sidebar = sidebar(
    title = "Navigation",
    actionButton("btn_overview", "Overview", icon = icon("tachometer-alt")),
    actionButton("btn_analysis", "Analyses", icon = icon("chart-line")),
    "---",
    helpText("Select a category to explore historical performance.")
  ),

  # B. The Main Canvas
  # We use a 'conditionalPanel' or logic to swap views.
  # For now, we focus on the "Overview" you drew.

  tagList(
    h2("Overview: Historical Statistics"),

    # --- ROW 1: DRIVER SECTION ---
    card(
      card_header("Driver Performance"),
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput("driver_select", "Select Driver:", choices = all_drivers),
          helpText("Compare career baselines.")
        ),
        # The Output Area (Right side of your arrow)
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Championships", value = textOutput("drv_champs"), showcase = icon("trophy")),
          value_box(title = "Race Wins", value = textOutput("drv_wins"), showcase = icon("flag-checkered")),
          value_box(title = "Avg Ranking", value = "3.2", showcase = icon("chart-bar"))
        )
      )
    ),

    # --- ROW 2: MANUFACTURER SECTION ---
    card(
      card_header("Manufacturer Performance"),
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput("team_select", "Select Manufacturer:", choices = all_teams)
        ),
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Championships", value = textOutput("team_champs"), showcase = icon("trophy")),
          value_box(title = "Race Wins", value = "102", showcase = icon("flag-checkered")),
          value_box(title = "Seasons in F1", value = "74", showcase = icon("calendar"))
        )
      )
    ),

    # --- ROW 3: CIRCUIT SECTION ---
    card(
      card_header("Circuit History"),
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput("circuit_select", "Select Circuit:", choices = all_circuits)
        ),
        layout_columns(
          # Column 1: The Stats
          card_body(
            h5("Track Records"),
            p(strong("Fastest Lap:"), "1:18.442 (Hamilton, 2020)"),
            p(strong("First Race:"), "1950"),
            p(strong("Fastest Quali Pace:"), "1:17.300")
          ),
          # Column 2: The Map Image
          card_body(
            plotOutput("circuit_map", height = "200px")
          )
        )
      )
    )
  )
)

# --- 3. SERVER LOGIC (The Brains) ---
server <- function(input, output, session) {

  # Reactive: Calculate Driver Stats
  # (This is where we will hook up your 'master_history' data later)
  output$drv_champs <- renderText({
    # Placeholder logic
    if(input$driver_select == "Max Verstappen") return("3")
    if(input$driver_select == "Lewis Hamilton") return("7")
    return("0")
  })

  output$drv_wins <- renderText({
    if(input$driver_select == "Max Verstappen") return("54")
    return("32")
  })

  # Reactive: Draw Circuit Map
  output$circuit_map <- renderPlot({
    # We will use f1dataR::plot_fastest_lap() or similar here
    ggplot() +
      geom_text(aes(x=0.5, y=0.5, label = paste("Map of", input$circuit_select))) +
      theme_void() +
      theme(panel.background = element_rect(fill = "#2b2b2b", color = NA),
            text = element_text(color = "white"))
  })
}

shinyApp(ui, server)
