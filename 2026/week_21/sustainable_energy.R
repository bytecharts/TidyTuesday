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
  filter(country_code %in% valid_iso3) |>
  mutate(region = countrycode::countrycode(country_code, "iso3c", "region", warn = FALSE)) |>
  filter(!is.na(region))

energy_output <- energy_countries |>
  transmute(
    country = country_name,
    year = yr,
    output_gwh = total_electricity_output_gigawatt_hours
  ) |>
  filter(!is.na(output_gwh), output_gwh > 0)

output_2010 <- energy_output |>
  filter(year == 2010) |>
  summarise(output_2010 = sum(output_gwh, na.rm = TRUE), .by = country)

top_countries <- energy_output |>
  summarise(total_output = sum(output_gwh, na.rm = TRUE), .by = country) |>
  left_join(output_2010, by = "country") |>
  mutate(output_2010 = tidyr::replace_na(output_2010, 0)) |>
  slice_max(order_by = total_output, n = 50, with_ties = FALSE) |>
  arrange(desc(output_2010))

losses_df <- energy_countries |>
  transmute(
    country = country_name,
    year = yr,
    losses_pct = transmission_and_distribution_losses_pct
  ) |>
  filter(country %in% top_countries$country, !is.na(losses_pct)) |>
  summarise(losses_pct = mean(losses_pct, na.rm = TRUE), .by = c(country, year)) |>
  mutate(
    country = forcats::fct_rev(factor(country, levels = top_countries$country)),
    year = factor(year)
  )

year_levels <- levels(losses_df$year)
x_end <- length(year_levels)
latest_year <- max(as.integer(as.character(losses_df$year)), na.rm = TRUE)
label_df <- losses_df |>
  mutate(year_num = as.integer(as.character(year))) |>
  filter(year_num == latest_year, losses_pct > 15) |>
  mutate(
    label = scales::percent(losses_pct, scale = 1, accuracy = 1),
    start_year = year_levels[1],
    end_year = year_levels[x_end]
  )

highlight_countries <- label_df |>
  distinct(country) |>
  mutate(highlight = TRUE)

axis_label_df <- losses_df |>
  distinct(country) |>
  left_join(highlight_countries, by = "country") |>
  mutate(
    highlight = tidyr::replace_na(highlight, FALSE),
    start_year = year_levels[1],
    label = as.character(country)
  )

p_heatmap <- ggplot(losses_df, aes(x = year, y = country, fill = losses_pct)) +
  geom_tile(color = NA) +
  geom_text(
    data = axis_label_df,
    aes(x = start_year, y = country, label = label, alpha = highlight),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = -0.55,
    size = 2.8,
    color = theme_fg,
    family = "FiraSans"
  ) +
  geom_label(
    data = label_df,
    aes(x = end_year, y = country, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    nudge_x = 0.55,
    size = 2.8,
    color = theme_fg,
    family = "FiraSans",
    linewidth = 0.25,
    label.r = unit(0, "lines"),
    label.padding = unit(0.12, "lines"),
    fill = NA
  ) +
  scale_fill_gradientn(
    colors = c(night_owlish_light$bg_soft, night_owlish_cat[4], night_owlish_cat[3]),
    labels = scales::percent_format(scale = 1),
    name = "T&D losses (%)"
  ) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Developing Countries Bear Higher Grid Losses",
    subtitle = "Transmission and distribution losses are higher across developing economies,<br> sorted by 2010 output with >15% loss highlighted",
    x = "Year",
    y = NULL,
    caption = caption_global("Energy | TidyTuesday (2026-05-26)", "21", "Sustainable Energy")
  ) +
  theme_base(base_size = 12) +
  theme(
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(30, 30, 30, 90),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 9),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(margin = margin(l=8)),
    legend.margin =  margin(l=18),
    legend.title = element_text(margin = margin(b=6, l=0)),
    legend.title.align = 0.5,
    legend.position = "right"
  )

ggsave(
  "sustainable_energy_heatmap.png",
  plot = p_heatmap,
  device = ragg::agg_png,
  width = 12,
  height = 10,
  dpi = 320,
  bg = theme_bg
)
