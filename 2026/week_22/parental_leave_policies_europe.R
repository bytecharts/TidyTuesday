library(tidyverse)
source("../theme/theme.R")

eplp <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv',
  show_col_types = FALSE
)

country_codes <- c(
  "AT",
  "BE",
  "CZ",
  "DE",
  "DK",
  "EE",
  "ES",
  "FI",
  "FR",
  "GR",
  "HU",
  "IE",
  "IT",
  "LT",
  "NL",
  "NO",
  "PL",
  "SE",
  "SI",
  "SK",
  "UK"
)
country_labels <- c(
  "Austria",
  "Belgium",
  "Czechia",
  "Germany",
  "Denmark",
  "Estonia",
  "Spain",
  "Finland",
  "France",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Lithuania",
  "Netherlands",
  "Norway",
  "Poland",
  "Sweden",
  "Slovenia",
  "Slovakia",
  "United Kingdom"
)

regions <- list(
  Anglo = c("IE", "UK"),
  Continental = c("AT", "BE", "DE", "FR", "NL"),
  Southern = c("ES", "GR", "IT"),
  EasternMiddle = c("CZ", "EE", "HU", "LT", "PL", "SI", "SK"),
  Nordic = c("DK", "FI", "NO", "SE")
)

region_of <- setNames(rep(names(regions), lengths(regions)), unlist(regions))
region_idx_map <- setNames(seq_along(regions), names(regions))

dat <- eplp |>
  filter(year %in% c(1970, 2024), mat_v_ld_ab >= 0, !is.na(mat_v_ld_ab)) |>
  mutate(
    r = sqrt(2 * mat_v_ld_ab / pi),
    region = region_of[country],
    region_rank = as.numeric(region_idx_map[region]),
    country_name = setNames(country_labels, country_codes)[country]
  )

n_t <- 80

semi_path <- function(r, rotation_deg, sign_y = 1) {
  theta <- seq(-pi / 2, pi / 2, length.out = n_t)
  bx <- r * cos(theta)
  by <- sign_y * r * sin(theta)
  rot <- rotation_deg * pi / 180
  tibble(
    x = bx * cos(rot) - by * sin(rot),
    y = bx * sin(rot) + by * cos(rot)
  )
}

angles <- c(`1970` = 90, `2024` = -90)

polys <- map_dfr(seq_len(nrow(dat)), function(i) {
  yr <- dat$year[i]
  semi_path(dat$r[i], angles[as.character(yr)], sign_y = 1) |>
    mutate(
      country_code = dat$country[i],
      year = yr,
      value = dat$mat_v_ld_ab[i]
    )
}) |>
  mutate(
    country_name = setNames(country_labels, country_codes)[country_code],
    region = region_of[country_code],
    region_rank = as.numeric(region_idx_map[region]),
    country_rank = match(country_code, unlist(regions)),
    sort_key = region_rank * 100 + country_rank
  )

country_colors <- setNames(
  rep(night_owlish_cat, length.out = length(country_codes)),
  setNames(country_labels, country_codes)[country_codes]
)

p <- polys |>
  ggplot(aes(x = x, y = y)) +
  geom_polygon(
    aes(fill = country_name),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_manual(values = country_colors, guide = "none") +
  coord_equal(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5)) +
  facet_wrap(~ reorder(country_name, sort_key), ncol = 3) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5F5F8", color = NA),
    panel.background = element_rect(fill = "#F5F5F8", color = NA),
    strip.text = element_blank(),
    panel.spacing = unit(2, "mm"),
    plot.margin = margin(2, 2, 2, 2)
  )

ggsave(
  "eplp_arc.png",
  p,
  device = ragg::agg_png,
  width = 8,
  height = 12,
  dpi = 320,
  bg = "#fff"
)
