# Packages
library(tidyverse)
library(geomtextpath)
source("../theme/theme.R")

# -----------------------------
# Load data
# -----------------------------
cities <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/cities.csv",
  show_col_types = FALSE
)
links <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/links.csv",
  show_col_types = FALSE
)

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  dlat <- (lat2 - lat1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) *
    sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

edges <- links |>
  left_join(
    cities |> select(id, lng, lat),
    by = c("source" = "id")
  ) |>
  rename(
    lng_source = lng,
    lat_source = lat
  ) |>
  left_join(
    cities |> select(id, lng, lat),
    by = c("target" = "id")
  ) |>
  rename(
    lng_target = lng,
    lat_target = lat
  ) |>
  mutate(distance_km = haversine_km(lng_source, lat_source, lng_target, lat_target))

city_edges <- edges |>
  select(city_id = source, distance_km) |>
  bind_rows(edges |> select(city_id = target, distance_km))

city_stats <- city_edges |>
  summarise(
    avg_km = mean(distance_km, na.rm = TRUE),
    .by = city_id
  ) |>
  left_join(cities |> select(id, name, continent), by = c("city_id" = "id")) |>
  filter(!is.na(continent))

continent_colors <- c(
  Africa = night_owlish_cat[2],
  Asia = night_owlish_cat[6],
  Europe = night_owlish_cat[1],
  `North America` = night_owlish_cat[3],
  Oceania = night_owlish_cat[5],
  `South America` = night_owlish_cat[4]
)

continent_levels <- names(continent_colors)
set.seed(19)
city_radial <- city_stats |>
  mutate(
    continent = factor(continent, levels = continent_levels),
    continent_index = as.numeric(continent),
    x_jitter = continent_index + runif(n(), -0.25, 0.25)
  )

continent_city_counts <- cities |>
  filter(!is.na(continent)) |>
  summarise(
    cities = n_distinct(id),
    .by = continent
  )

continent_axis_labels <- continent_city_counts |>
  mutate(continent = factor(continent, levels = continent_levels)) |>
  arrange(continent) |>
  mutate(
    label = sprintf(
      "<span style='color:%s'>%s</span><br><span style='color:%s;font-size:14px;'>%s cities</span>",
      continent_colors[continent],
      continent,
      night_owlish_light$gray,
      scales::comma(cities)
    )
  )

continent_axis_labels <- setNames(
  continent_axis_labels$label,
  as.character(continent_axis_labels$continent)
)

continent_means <- city_radial |>
  summarise(
    mean_km = mean(avg_km, na.rm = TRUE),
    .by = c(continent, continent_index)
  ) |>
  left_join(continent_city_counts, by = "continent")

arc_points <- continent_means |>
  mutate(
    arc = purrr::map(
      continent_index,
      ~ tibble(x = seq(.x - 0.45, .x + 0.45, length.out = 120))
    )
  ) |>
  select(continent, continent_index, mean_km, cities, arc) |>
  unnest(arc) |>
  mutate(
    y = mean_km,
    x = if_else(
      continent == "Europe",
      x + if_else(x > continent_index, 0.07, -0.07),
      x
    ),
    label = paste0(
      scales::comma(mean_km),
      " km"
    )
  )

ring_breaks <- pretty(city_radial$avg_km, n = 5)
ring_path <- tibble(
  x = rep(seq(0.5, length(continent_levels) + 0.5, length.out = 200),
    times = length(ring_breaks)
  ),
  y = rep(ring_breaks, each = 200),
  label = rep(paste0(scales::comma(ring_breaks), " km"), each = 200)
)

top_continent_cities <- city_radial |>
  group_by(continent, continent_index) |>
  slice_max(avg_km, n = 1, with_ties = FALSE) |>
  mutate(
    label = name,
    x_point = x_jitter,
    y_point = avg_km
  ) |>
  ungroup()

overall_mean_km <- mean(continent_means$mean_km, na.rm = TRUE)
legend_arc <- tibble(
  x = seq(0.6, 1.4, length.out = 120),
  y = overall_mean_km,
  label = paste0(scales::comma(overall_mean_km), " km")
)
legend_path <- tibble(
  x = seq(0.55, 1.45, length.out = 120),
  y = overall_mean_km
)

# -----------------------------
# Plot
# -----------------------------
p_radial <- city_radial |>
  ggplot(aes(x = x_jitter, y = avg_km, color = continent)) +
  geom_hline(
    yintercept = ring_breaks,
    color = night_owlish_light$gray,
    linewidth = 0.25
  ) +
  geom_path(
    data = arc_points,
    aes(x = x, y = y, color = continent, group = continent),
    inherit.aes = FALSE,
    linewidth = 1.4
  ) +
  geom_labelpath(
    data = ring_path,
    aes(x = x, y = y, label = label, group = label),
    inherit.aes = FALSE,
    text_only = TRUE,
    family = "FiraSans",
    size = 3.2,
    boxlinewidth = 0,
    color = night_owlish_light$gray
  ) +
  geom_point(alpha = 0.70, size = 2.2) +
  geom_labelpath(
    data = arc_points,
    aes(x = x, y = y, label = label, color = continent, group = continent),
    inherit.aes = FALSE,
    text_only = TRUE,
    family = "FiraSans",
    size = 3.75,
    fontface = "bold"
  ) +
  geom_text(
    data = top_continent_cities,
    aes(x = x_point, y = y_point, label = label, color = continent),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    nudge_x = -0.02,
    nudge_y = 800
  ) +
  coord_polar(clip = "off") +
  scale_color_manual(values = continent_colors, guide = "none") +
  scale_y_continuous(
    breaks = ring_breaks,
    labels = scales::comma_format()
  ) +
  scale_x_continuous(
    breaks = seq_along(continent_levels),
    labels = continent_axis_labels,
    limits = c(0.5, length(continent_levels) + 0.5)
  ) +
  labs(
    title = "Twinned Cities By Continent",
 subtitle = "Cities are grouped by continent and positioned by average twin-city distance.<br>Arcs denote the mean distance for each continent.",
    x = "",
    y = "Average distance (km)",
    color = "Continent",
    caption = caption_global("Twin Cities | TidyTuesday (2026-05-12)", "19", "Twin Cities")
  ) +
  theme_base(base_size = 12) +
  theme(
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA),
    axis.text.x = element_markdown(size = 16),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave(
  "twin_cities_city_radial.png",
  plot = p_radial,
  device = ragg::agg_png,
  width = 14,
  height = 12,
  dpi = 320,
  bg = theme_bg
)
