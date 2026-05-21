library(tidyverse)
library(rvest)
library(ggrepel)
source("../theme/theme.R")

member_participation_stats_by_country <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/member_participation_stats_by_country.csv",
  show_col_types = FALSE
)
metadata_coverage_stats_by_country <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/metadata_coverage_stats_by_country.csv",
  show_col_types = FALSE
)

region_acry_url <- "https://esgdata.worldbank.org/about/faq?lang=en"
region_acry_raw_html <- read_html(region_acry_url)
wb_codes <- region_acry_raw_html |>
  html_elements(".datatable") |>
  html_table() |>
  purrr::pluck(2)


country_iso3_codes_url <- "https://wits.worldbank.org/WITS/wits/WITSHELP/Content/Codes/Country_Codes.htm"
country_iso3_codes_raw_html <- read_html(country_iso3_codes_url)
country_codes <- country_iso3_codes_raw_html |>
  html_elements(".WT1") |>
  html_table() |>
  purrr::pluck(1)

wb_region_codes <- wb_codes |>
  rename(
    region_id = Abbreviation,
    region = Description
  ) |>
  select(region_id, region)

country_iso3_codes <- country_codes |>
  slice(-(1:2)) |>
  set_names(c("country", "iso3_code", "code")) |>
  select(country, iso3_code)

member_participation_stats_by_country <- member_participation_stats_by_country |>
  left_join(wb_region_codes, by = "region_id") |>
  left_join(country_iso3_codes, by = "iso3_code")

metadata_coverage_stats_by_country <- metadata_coverage_stats_by_country |>
  left_join(wb_region_codes, by = "region_id") |>
  left_join(country_iso3_codes, by = "iso3_code")

readr::write_csv(
  member_participation_stats_by_country,
  "./data/member_participation_stats_by_country_full.csv"
)
readr::write_csv(
  metadata_coverage_stats_by_country,
  "./data/metadata_coverage_stats_by_country_full.csv"
)

member_by_year <- member_participation_stats_by_country |>
  mutate(year = lubridate::year(current_up_to)) |>
  filter(year >= 2018) |>
  group_by(iso3_code, year) |>
  filter(current_up_to == max(current_up_to, na.rm = TRUE)) |>
  ungroup()

metadata_by_year <- metadata_coverage_stats_by_country |>
  mutate(year = lubridate::year(current_up_to)) |>
  filter(year >= 2018) |>
  group_by(iso3_code, year) |>
  filter(current_up_to == max(current_up_to, na.rm = TRUE)) |>
  ungroup()

doi_totals <- metadata_by_year |>
  summarise(n_dois = sum(n_dois, na.rm = TRUE), .by = c(iso3_code, year))

quadrant_df <- member_by_year |>
  select(iso3_code, country, region_id, region, total_members, year) |>
  left_join(doi_totals, by = c("iso3_code", "year")) |>
  mutate(dois_per_member = n_dois / total_members) |>
  filter(is.finite(dois_per_member), total_members > 0, dois_per_member > 0)

readr::write_csv(
  quadrant_df |>
    select(year, iso3_code, country, region, total_members, n_dois, dois_per_member),
  "./data/crossref_quadrant_by_year.csv"
)

region_colors <- c(
  "East Asia & Pacific" = night_owlish_cat[6],
  "Europe & Central Asia" = night_owlish_cat[1],
  "Latin America & the Caribbean" = night_owlish_cat[4],
  "Middle East, North Africa, Afghanistan & Pakistan" = night_owlish_cat[5],
  "North America" = night_owlish_cat[3],
  "South Asia" = night_owlish_cat[9],
  "Sub-Saharan Africa" = night_owlish_cat[2]
)

quadrant_df <- quadrant_df |>
  mutate(region = factor(region, levels = names(region_colors)))

medians_by_year <- quadrant_df |>
  summarise(
    x_median = median(total_members, na.rm = TRUE),
    y_median = median(dois_per_member, na.rm = TRUE),
    .by = year
  )

floor_sigfig <- function(x) {
  x <- as.numeric(x)
  power <- 10 ^ floor(log10(x))
  out <- floor(x / power) * power
  out[!is.finite(out)] <- NA_real_
  out
}

size_breaks <- quantile(
  quadrant_df$dois_per_member,
  probs = c(0.2, 0.5, 0.8, 0.95),
  na.rm = TRUE
)
size_breaks <- floor_sigfig(size_breaks)
size_breaks <- sort(unique(size_breaks[!is.na(size_breaks)]))

quadrant_df <- quadrant_df |>
  left_join(medians_by_year, by = "year") |>
  mutate(
    members_group = if_else(total_members >= x_median, "high", "low"),
    dois_group = if_else(dois_per_member >= y_median, "high", "low"),
    quadrant = paste(members_group, dois_group, sep = "_")
  )

set.seed(20)
label_candidates <- quadrant_df |>
  group_by(year, quadrant) |>
  slice_max(order_by = n_dois, n = 12, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    label_x = total_members * exp(runif(n(), -0.08, 0.08)),
    label_y = dois_per_member * exp(runif(n(), -0.08, 0.08))
  )

latest_year <- max(quadrant_df$year, na.rm = TRUE)
quadrant_latest <- quadrant_df |>
  filter(year == latest_year)
medians_latest <- medians_by_year |>
  filter(year == latest_year)
label_candidates_latest <- label_candidates |>
  filter(year == latest_year)

build_quadrant_base <- function(data, medians, labels) {
  ggplot(
    data,
    aes(x = total_members, y = dois_per_member, color = region, size = dois_per_member)
  ) +
  geom_vline(
    data = medians,
    aes(xintercept = x_median),
    color = night_owlish_light$gray,
    linewidth = 0.4,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_hline(
    data = medians,
    aes(yintercept = y_median),
    color = night_owlish_light$gray,
    linewidth = 0.4,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_point(
    alpha = 0.7
  ) +
  geom_text_repel(
    data = labels,
    aes(x = label_x, y = label_y, label = country, color = region),
    inherit.aes = FALSE,
    family = "FiraSans",
    size = 2.5,
    alpha = 1,
    vjust = 0,
    hjust = -0.1,
    box.padding = 0.2,
    point.padding = 0.1,
    segment.color = night_owlish_light$gray,
    segment.size = 0.3,
    segment.alpha = 0.6,
    max.overlaps = Inf,
    show.legend = FALSE
  )
}

p_quadrant <- build_quadrant_base(quadrant_latest, medians_latest, label_candidates_latest) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_size_continuous(
    range = c(1, 18),
    name = "DOIs per member",
    breaks = size_breaks,
    labels = scales::comma_format(),
    guide = guide_legend(order = 1, override.aes = list(alpha = 0.9))
  ) +
  scale_color_manual(values = region_colors, drop = FALSE) +
  guides(
    color = guide_legend(order = 2, override.aes = list(size = 4, alpha = 1))
  ) +
  labs(
    title = "Crossref Participation Quadrants",
    subtitle = paste("Year:", latest_year, "| Quadrants split by yearly medians"),
    x = "Total members (log scale)",
    y = "DOIs per member (log scale)",
    color = "Region",
    caption = caption_global("Crossref | TidyTuesday (2026-05-19)", "20", "Crossref")
  ) +
  theme_base(base_size = 12) +
  theme(
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA),
    axis.title.y = element_text(
      size = 12,
      color = theme_fg,
      margin = margin(r = 10),
      angle = 90,
      vjust = 0.5
    ),
    panel.grid.major = element_line(
      color = night_owlish_light$bg_soft,
      linewidth = 0.3
    ),
    legend.position = "right"
  )

ggsave(
  "crossref_quadrant.png",
  plot = p_quadrant,
  device = ragg::agg_png,
  width = 12,
  height = 9,
  dpi = 320,
  bg = theme_bg
)
