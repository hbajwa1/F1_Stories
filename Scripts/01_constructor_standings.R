library(tidyverse)
library(f1dataR)
library(glue)
library(ggtext)

source("R/f1_helpers.R")

# --- 1. BUILD THE HISTORICAL MASTER FILE ---
years_to_analyze <- c(2022, 2023, 2024, 2025)
master_history <- map_dfr(years_to_analyze, build_master_season_data)

# 2. CALCULATE THE INDEX
tech_index <- pace_history %>%
  left_join(weights_table, by = "season") %>%
  # Helper to fix names (e.g., "racing_bulls" -> "RB")
  rowwise() %>%
  mutate(clean_name = translate_constructor(constructor_id, season, "clean_name")) %>%
  ungroup() %>%

  # The Weighted Average Calculation
  group_by(clean_name) %>%
  summarise(
    # Formula: Sum(Gap * Weight) / Sum(Weights)
    weighted_deficit = sum(avg_gap_pct * weight) / sum(weight),
    years_active = n() # Data check
  ) %>%
  arrange(weighted_deficit) # Ascending (Smaller gap is better)

# 3. VISUALIZATION
# We want Short Bars (Low Deficit) at the top
index_plot <- tech_index %>%
  slice_head(n = 10) %>% # Top 10
  ggplot(aes(x = reorder(clean_name, -weighted_deficit), y = weighted_deficit)) +

  # The Bars
  geom_col(aes(fill = clean_name), width = 0.65) +
  coord_flip() +

  # Get colors from our helper (or use scale_fill_identity if you added colors to the DF)
  # For now, let's assume we map them manually or fetch them
  scale_fill_manual(values = c(
    "Red Bull Racing" = "#3671C6", "McLaren" = "#FF8000", "Ferrari" = "#E80020",
    "Mercedes" = "#27F4D2", "Aston Martin" = "#229971", "Alpine" = "#0093CC",
    "Williams" = "#64C4FF", "Racing Bulls" = "#6692FF", "Audi Sauber" = "#52E252",
    "Haas" = "#B6BABD", "Kick Sauber" = "#52E252"
  )) +

  theme_dark_f1(axis_marks = TRUE) +

  # Labels
  labs(
    title = "THE 2026 TECHNICAL COMPETENCY INDEX",
    subtitle = "Weighted Avg. Qualifying Deficit to Pole (2022-2025)",
    y = "Average Performance Deficit (%)",
    x = NULL,
    caption = "Methodology: Pure qualifying pace weighted by season recency (40% 2025 ... 10% 2022).\nWet sessions included."
  ) +

  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "white", face = "bold", size = 11),
    panel.grid.major.x = element_line(color = "gray20", linetype = "dashed"),
    panel.grid.major.y = element_blank()
  )

# 4. SAVE
index_plot
ggsave("output/tech_index_2026.png", index_plot, width = 9, height = 6, dpi = 300)
