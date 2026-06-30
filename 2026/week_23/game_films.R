# Packages ---------------------------------------------------------------
library(tidyverse)
library(ggtext)

source("../theme/theme.R")
source("../utils/utils.R")

# Constants ---------------------------------------------------------------

TITLE <- glue::glue(
  "The rise of <span style='color:#F07178;'>English</span> video game films"
)

SUBTITLE <- glue::glue(
  "Among theatrical releases, <span style='color:#2E86AB;'>Japanese</span> films led for decades, ",
  "<span style='color:#F07178;'>English</span> films became the largest group in the 2020s and account for most upcoming releases, ",
  "while <span style='color:#7FDBCA;'>Mandarin/Cantonese</span> films emerged in the 2010s"
)


OUTPUT <- "game_films_by_language.png"


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

# Data ------------------------------------------------------------------------

game_films <- tt_cache(2026, 23)

plot_data <- game_films %>%
  filter(category == "Theatrical releases") %>%
  mutate(
    year = year(release_date),
    decade = case_when(
      !is.na(year) ~ as.character((year %/% 10) * 10),
      release_date_raw == "TBA" ~ "TBA",
      release_date_raw == "Undated" ~ "Undated",
      release_date_raw == "In development" ~ "In Development",
      TRUE ~ "Other Unknown"
    )
  ) %>%
  count(decade, subcategory, sort = TRUE)

plot_data <- plot_data %>%
  mutate(
    decade_label = if_else(
      grepl("^[0-9]+$", decade),
      paste0(decade, "s"),
      decade
    )
  )


print(plot_data)
print(summary(plot_data$n))

# Plot ------------------------------------------------------------------------

game_films_plot <- ggplot(
  plot_data,
  aes(x = factor(decade_label), y = n, fill = subcategory)
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
    lineheight = 0.9,
    fontface = "bold"
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
  filename = OUTPUT,
  device = ragg::agg_png,
  width = 16,
  height = 20,
  dpi = 340,
  bg = "#fff"
)
# Animated Version --------------------------------------------------------
decades <- plot_data %>%
  distinct(decade_label) %>%
  pull(decade_label)

plot_data_anim <- purrr::map_dfr(
  seq_along(decades),
  \(i) {
    plot_data %>%
      filter(decade_label %in% decades[1:i]) %>%
      mutate(current_decade = decades[i])
  }
)

library(gganimate)

game_films_plot_anim <-
  ggplot(
    plot_data_anim,
    aes(
      x = factor(decade_label, levels = decades),
      y = n,
      fill = subcategory
    )
  ) +
  geom_col(
    color = "black",
    linewidth = 2,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 8
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
  ) +
  transition_states(
    current_decade,
    transition_length = 2,
    state_length = 2
  ) +
  shadow_mark(
    past = TRUE,
    future = FALSE,
    alpha = 1
  ) +
  enter_grow() +
  exit_fade()


anim_save(
  "game_films_by_language.gif",
  animate(
    game_films_plot_anim,
    width = 5440,
    height = 6800,
    res = 340,
    fps = 30,
    duration = 8,
    end_pause = 60,
    renderer = gifski_renderer()
  )
)
