# Packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ragg)
library(gifski)
library(ggtext)

source("../theme/theme.R")
source("../utils/utils.R")

# Constants ---------------------------------------------------------------

TITLE <- glue(
  "The rise of <span style='color:#F07178;'>English</span> video game films"
)

SUBTITLE <- glue(
  "Among theatrical releases, <span style='color:#2E86AB;'>Japanese</span> films led for decades, ",
  "<span style='color:#F07178;'>English</span> films became the largest group in the 2020s and account for most upcoming releases, ",
  "while <span style='color:#7FDBCA;'>Mandarin/Cantonese</span> films emerged in the 2010s"
)

CAPTION <- caption_global(
  "Wikipedia - List of films based on video games",
  "23",
  "Game Films"
)

SUBCATEGORY_COLORS <- c(
  "English" = "#F07178",
  "Japanese" = "#2E86AB",
  "Mandarin/Cantonese" = "#7FDBCA"
)

OUTPUT_GIF <- "game_films_by_language.gif"

# Data --------------------------------------------------------------------

game_films <- tt_cache(2026, 23)

plot_data <- game_films %>%
  filter(category == "Theatrical releases") %>%
  mutate(
    year = lubridate::year(release_date),
    decade = case_when(
      !is.na(year) ~ as.character((year %/% 10) * 10),
      release_date_raw == "TBA" ~ "TBA",
      release_date_raw == "Undated" ~ "Undated",
      release_date_raw == "In development" ~ "In Development",
      TRUE ~ "Other Unknown"
    )
  ) %>%
  count(decade, subcategory, sort = TRUE) %>%
  mutate(
    decade_label = if_else(
      grepl("^[0-9]+$", decade),
      paste0(decade, "s"),
      decade
    )
  )

# Frame setup -------------------------------------------------------------

frame_dir <- tempfile("game_film_frames_")
dir.create(frame_dir)

decades <- plot_data %>%
  distinct(decade_label) %>%
  pull(decade_label)

# Build frames ------------------------------------------------------------

for (i in seq_along(decades)) {
  current_data <- plot_data %>%
    filter(decade_label %in% decades[1:i])

  p <- ggplot(
    current_data,
    aes(
      x = factor(decade_label, levels = decades),
      y = n,
      fill = subcategory
    )
  ) +
    geom_col() +
    geom_col(
      color = "black",
      linewidth = 2,
      alpha = 0.95
    ) +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = 0.5),
      size = 8,
      color = "white",
      fontface = "bold",
      lineheight = 0.9
    ) +
    scale_fill_manual(values = SUBCATEGORY_COLORS) +
    scale_y_continuous(
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = TITLE,
      subtitle = SUBTITLE,
      caption = CAPTION,
      x = "Release Decade",
      y = "Number of Films"
    ) +
    theme_base() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      axis.title.x = element_text(hjust = 0.3)
    )

  ggsave(
    filename = file.path(
      frame_dir,
      sprintf("frame_%03d.png", i)
    ),
    plot = p,
    device = ragg::agg_png,
    width = 16,
    height = 20,
    dpi = 340,
    bg = "#fff"
  )
}

# GIF sequence ------------------------------------------------------------

frame_files <- list.files(
  frame_dir,
  pattern = "\\.png$",
  full.names = TRUE
)

gif_frames <- c(
  rep(frame_files, each = 15), # ~1 second per decade at 15 fps
  rep(tail(frame_files, 1), 45) # 3-second pause on final frame
)

# Render GIF --------------------------------------------------------------

gifski(
  png_files = gif_frames,
  gif_file = OUTPUT_GIF,
  width = 5440,
  height = 6800,
  delay = 1 / 15
)

# Cleanup -----------------------------------------------------------------

unlink(frame_dir, recursive = TRUE)
