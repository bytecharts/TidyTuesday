library(tidyverse)
source("../theme/theme.R")

energy <- readr::read_csv(
  "https://github.com/rfordatascience/tidytuesday/raw/refs/heads/main/data/2026/2026-05-26/energy_cleaned.csv",
  show_col_types = FALSE
)

valid_iso3 <- unique(countrycode::codelist$iso3c)
valid_iso3 <- valid_iso3[!is.na(valid_iso3)]

energy_countries <- energy |>
  mutate(country_code = dplyr::recode(country_code, KSV = "XKX")) |>
  filter(country_code %in% valid_iso3)

energy_output <- energy_countries |>
  transmute(
    country = country_name,
    output_gwh = total_electricity_output_gigawatt_hours
  ) |>
  filter(!is.na(output_gwh), output_gwh > 0)

top_countries <- energy_output |>
  summarise(total_output = sum(output_gwh, na.rm = TRUE), .by = country) |>
  slice_max(order_by = total_output, n = 20, with_ties = FALSE)

losses_df <- energy_countries |>
  transmute(
    country = country_name,
    year = yr,
    losses_pct = transmission_and_distribution_losses_pct
  ) |>
  filter(!is.na(losses_pct), country %in% top_countries$country) |>
  summarise(losses_pct = mean(losses_pct, na.rm = TRUE), .by = c(country, year))

country_order <- losses_df |>
  summarise(mean_loss = mean(losses_pct, na.rm = TRUE), .by = country) |>
  arrange(mean_loss)

year_levels <- sort(unique(losses_df$year))
losses_df <- losses_df |>
  mutate(
    country = factor(country, levels = country_order$country),
    year_index = match(year, year_levels),
    country_index = as.numeric(country)
  )

tile_corners <- tibble(
  corner = 1:4,
  dx = c(-0.5, 0.5, 0.5, -0.5),
  dy = c(-0.5, -0.5, 0.5, 0.5)
)

tile_polys <- losses_df |>
  mutate(tile_id = row_number()) |>
  tidyr::crossing(tile_corners) |>
  mutate(
    x = year_index + (dx - dy) / sqrt(2),
    y = country_index + (dx + dy) / sqrt(2)
  )

p_art <- ggplot(tile_polys, aes(x = x, y = y, group = tile_id, fill = losses_pct)) +
  geom_polygon(color = NA) +
  coord_equal() +
  scale_fill_gradientn(
    colors = c(night_owlish_light$bg_soft, night_owlish_cat[4], night_owlish_cat[3]),
    guide = "none"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "sustainable_energy_art.png",
  plot = p_art,
  device = ragg::agg_png,
  width = 12,
  height = 14,
  dpi = 320,
  bg = theme_bg
)
