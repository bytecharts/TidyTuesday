# Packages ----------------------------------------------------------------

library(ggimage)
library(magick)
library(rsvg)
library(tidyverse)
library(rlang)
library(scales)

source("../theme/theme.R")

# Constants ---------------------------------------------------------------

purple <- "#8B7CF0"
purple_dark <- "#6C5CE7"
red_down <- "#E57373"
primary <- "#2A2F3ABC"
primary_title <- "#2A2F3A"

DATA_URL <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv"
FLAG_DIR <- "flag_cache"
OUTPUT <- "eplp_maternal_leave_1970_to_current.png"

COUNTRIES <- c(
  AT = "Austria",
  BE = "Belgium",
  CZ = "Czechia",
  DE = "Germany",
  DK = "Denmark",
  EE = "Estonia",
  ES = "Spain",
  FI = "Finland",
  FR = "France",
  GR = "Greece",
  HU = "Hungary",
  IE = "Ireland",
  IT = "Italy",
  LT = "Lithuania",
  NL = "Netherlands",
  NO = "Norway",
  PL = "Poland",
  SE = "Sweden",
  SI = "Slovenia",
  SK = "Slovakia",
  UK = "United Kingdom"
)


SUBTITLE <- paste0(
  "Mandatory and voluntary maternity leave weeks granted to new mothers across 21 European countries, <br>from ",
  "<span style='color:#6C5CE7B3;'> 1970</span>",
  " to ",
  "<span style='color:#6C5CE7;'> 2024</span>"
)


# Flag helpers ------------------------------------------------------------

flag_cdn_code <- function(code) if_else(code == "UK", "gb", tolower(code))
flag_url <- function(code) {
  paste0("https://flagcdn.com/w80/", flag_cdn_code(code), ".png")
}

circle_mask_svg <- function(size = 80) {
  path <- tempfile(fileext = ".svg")
  writeLines(
    sprintf(
      "<svg xmlns='http://www.w3.org/2000/svg' width='%1$d' height='%1$d' viewBox='0 0 %1$d %1$d'>
         <circle cx='%2$d' cy='%2$d' r='%2$d' fill='white'/>
       </svg>",
      size,
      size / 2L
    ),
    path
  )
  path
}

clip_flag <- function(url, dest, size = 80, mask_path = circle_mask_svg(size)) {
  flag <- magick::image_read(url) |>
    magick::image_resize(sprintf("%dx%d^", size, size)) |>
    magick::image_crop(sprintf("%dx%d+0+0", size, size))

  mask <- rsvg::rsvg_png(mask_path, width = size, height = size) |>
    magick::image_read()

  magick::image_composite(flag, mask, operator = "copyopacity") |>
    magick::image_write(dest, format = "png")

  dest
}

build_flag_data <- function(codes, dir = FLAG_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  tibble(
    country = codes,
    country_name = COUNTRIES[codes],
    flag_path = file.path(dir, paste0(flag_cdn_code(codes), ".png"))
  ) |>
    mutate(flag_path = map2_chr(flag_url(country), flag_path, clip_flag))
}

# Data preparation --------------------------------------------------------

load_data <- function(url = DATA_URL) {
  readr::read_csv(url, show_col_types = FALSE)
}

#' Earliest vs 2024 endpoints for one leave variable, one row per country.
#' flip_sign = TRUE negates values so before-birth leave extends left.
extract_endpoints <- function(
  data,
  value_col,
  flip_sign = FALSE,
  order_levels = NULL
) {
  val <- sym(value_col)

  earliest <- data |>
    filter(country %in% names(COUNTRIES), !!val > 0) |>
    group_by(country) |>
    slice_min(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(country, earliest_year = year, earliest_value = !!val)

  latest <- data |>
    filter(country %in% names(COUNTRIES), year == 2024, !is.na(!!val)) |>
    transmute(country, latest_value = !!val)

  left_join(earliest, latest, by = "country") |>
    mutate(
      country_name = COUNTRIES[country],
      earliest_plot = if (flip_sign) -earliest_value else earliest_value,
      latest_plot = if (flip_sign) -latest_value else latest_value,
      country_name = if (is.null(order_levels)) {
        fct_reorder(
          country_name,
          coalesce(latest_value, earliest_value),
          .desc = TRUE
        )
      } else {
        factor(country_name, levels = order_levels)
      }
    )
}

#' One row per country per direction, with change metadata and track extents.
build_change_data <- function(data) {
  totals <- data |>
    mutate(
      mat_t_ld_ab = coalesce(mat_m_ld_ab, 0) + coalesce(mat_v_ld_ab, 0),
      mat_t_ld_bb = coalesce(mat_m_ld_bb, 0) + coalesce(mat_v_ld_bb, 0)
    )

  after <- extract_endpoints(totals, "mat_t_ld_ab")
  before <- extract_endpoints(
    totals,
    "mat_t_ld_bb",
    flip_sign = TRUE,
    order_levels = levels(after$country_name)
  )

  combined <- bind_rows(
    mutate(after, direction = "After birth"),
    mutate(before, direction = "Before birth")
  ) |>
    mutate(
      delta = latest_plot - earliest_plot,
      delta_label = number(delta, accuracy = 1, style_positive = "plus"),
      direction_change = case_when(
        delta > 0 ~ "up",
        delta < 0 ~ "down",
        TRUE ~ "same"
      ),
      year_label = if_else(
        earliest_year == 1970,
        "",
        paste0(earliest_year, "*")
      ),
      bar_min = pmin(earliest_plot, latest_plot, na.rm = TRUE),
      bar_max = pmax(earliest_plot, latest_plot, na.rm = TRUE)
    )

  # Per-country track extents (grey pill spans both directions)
  extents <- combined |>
    group_by(country_name) |>
    summarise(
      track_min = min(bar_min, na.rm = TRUE),
      track_max = max(bar_max, na.rm = TRUE),
      .groups = "drop"
    )

  left_join(combined, extents, by = "country_name")
}

# Plot --------------------------------------------------------------------

build_plot <- function(change_data, flag_data) {
  after_max <- max(
    filter(change_data, direction == "After birth")$bar_max,
    na.rm = TRUE
  )
  before_min <- min(
    filter(change_data, direction == "Before birth")$bar_min,
    na.rm = TRUE
  )

  ggplot(change_data, aes(y = country_name)) +

    # 0. Birth line — first so everything renders on top
    geom_vline(xintercept = 0, color = base_colors$neutral, linewidth = 1.5) +

    # 1. Grey track (full country range)
    geom_segment(
      aes(x = track_min, xend = track_max, yend = country_name),
      colour = "grey82",
      linewidth = 12,
      lineend = "round"
    ) +

    # 2. Coloured segment (changed interval only)
    geom_segment(
      aes(
        x = bar_min,
        xend = bar_max,
        yend = country_name,
        colour = direction_change
      ),
      linewidth = 12,
      lineend = "round",
      na.rm = TRUE
    ) +
    scale_colour_manual(
      values = c(up = purple, down = red_down, same = "grey62"),
      guide = "none"
    ) +

    # 3. Earliest circle (open)
    geom_point(
      aes(x = earliest_plot),
      shape = 21,
      size = 8,
      fill = "white",
      colour = purple_dark,
      stroke = 1.5,
      na.rm = TRUE
    ) +

    # 4. 2024 circle (filled)
    geom_point(
      aes(x = latest_plot),
      shape = 21,
      size = 8,
      fill = purple_dark,
      colour = purple_dark,
      na.rm = TRUE
    ) +

    # 3b. Value inside earliest circle
    geom_text(
      aes(x = earliest_plot, label = round(abs(earliest_value))),
      size = 4.5,
      colour = purple_dark,
      family = "FiraSans",
      fontface = "bold",
      na.rm = TRUE
    ) +

    # 4b. Value inside 2024 circle
    geom_text(
      aes(x = latest_plot, label = round(abs(latest_value))),
      size = 4.5,
      colour = "white",
      family = "FiraSans",
      fontface = "bold",
      na.rm = TRUE
    ) +

    # 5a. Non-1970 year label — centered below segment
    geom_text(
      data = filter(change_data, year_label != ""),
      aes(x = (bar_min + bar_max) / 2, label = year_label),
      vjust = 3.2,
      size = 4.5,
      colour = purple,
      alpha = 0.75,
      family = "FiraSans",
      na.rm = TRUE
    ) +

    # 6a. After-birth delta (right)
    geom_text(
      data = filter(change_data, direction == "After birth"),
      aes(x = bar_max + 2, label = delta_label, colour = direction_change),
      hjust = 0,
      size = 4.5,
      family = "FiraSans",
      na.rm = TRUE
    ) +

    # 6b. Before-birth delta (left)
    geom_text(
      data = filter(change_data, direction == "Before birth"),
      aes(x = bar_min - 2, label = delta_label, colour = direction_change),
      hjust = 1,
      size = 4.5,
      family = "FiraSans",
      na.rm = TRUE
    ) +

    # 7. Flag circle background
    geom_point(
      data = flag_data,
      aes(x = 0, y = country_name),
      shape = 21,
      size = 10,
      fill = "white",
      color = night_owlish_light$bg_soft,
      stroke = 0.4
    ) +

    # 8. Flag image
    ggimage::geom_image(
      data = flag_data,
      aes(x = 0, y = country_name, image = flag_path),
      size = 0.019
    ) +

    scale_x_continuous(
      limits = c(before_min - 8, after_max + 10),
      breaks = pretty(c(before_min, after_max), n = 6),
      labels = function(x) ifelse(x == 0, "Day of Birth", abs(x))
    ) +
    scale_y_discrete(expand = expansion(add = c(1.2, 1.2))) +
    coord_cartesian(clip = "off") +

    labs(
      title = "How Has Maternity Leave Changed Across Europe Since 1970?",
      subtitle = SUBTITLE,
      caption = caption_global(
        'S. Spitzer et al., "The European Parenting Leave Policies (EPLP) Dataset". Zenodo, Nov. 19, 2025. doi: 10.5281/zenodo.17648712.',
        "22",
        "European Parenting Leave Policies"
      ),
      x = "Weeks of maternity leave",
      y = NULL
    ) +

    theme_base() +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = primary_title),
      plot.title = element_textbox_simple(
        family = theme_title_family,
        color = primary_title,
        face = "bold",
        size = 32,
        hjust = 0,
        width = unit(1, "npc"),
        padding = margin(5, 1, 5, 1),
        margin = margin(b = 6),
        fill = "white",
        box.color = "white"
      ),
      plot.background = element_rect(fill = night_owlish_light$bg, color = NA),
      panel.grid.major.x = element_line(color = night_owlish_light$bg_soft),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(
        angle = 0,
        vjust = 1,
        hjust = 0.5,
        size = 16,
        color = primary
      ),
      axis.text.y = element_text(
        color = primary,
        angle = 0,
        vjust = 1,
        hjust = 0,
        size = 16
      ),
      axis.title.x = element_text(
        size = 18,
        color = primary_title,
        hjust = 0.225
      ),
      plot.subtitle = element_markdown(
        color = primary,
        size = 20,
        margin = margin(b = 12)
      ),
      plot.margin = margin(40, 20, 20, 40)
    )
}

# Annotations -------------------------------------------------------------

add_annotations <- function(p, change_data) {
  es <- filter(change_data, country == "ES", direction == "After birth")
  de <- filter(change_data, country == "DE", direction == "After birth")
  se <- filter(change_data, country == "SE", direction == "After birth")

  ann_curves <- tibble(
    x = c(es$earliest_plot, de$latest_plot, (se$bar_min + se$bar_max) / 2),
    y = c(
      as.numeric(es$country_name) - 0.18,
      as.numeric(de$country_name) + 0.18,
      as.numeric(se$country_name) - 0.5
    ),
    xend = c(30, 25, 20),
    yend = c(
      as.numeric(es$country_name) - 2.5,
      as.numeric(de$country_name) + 1.5,
      as.numeric(se$country_name) - 2.5
    )
  )

  ann_text <- tibble(
    x = c(31, 26, 21),
    y = c(
      as.numeric(es$country_name) - 2.5,
      as.numeric(de$country_name) + 1.5,
      as.numeric(se$country_name) - 2.5
    ),
    label = c(
      "Open circle = earliest available data",
      "Filled circle = 2024 data",
      "* Data begins after 1970\nfor a handful of countries"
    )
  )

  p +
    geom_curve(
      data = ann_curves,
      aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0.25,
      angle = 20,
      arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
      color = primary_title,
      linewidth = 0.4
    ) +
    geom_text(
      data = ann_text,
      aes(x = x, y = y, label = label),
      hjust = 0,
      vjust = 0.5,
      color = primary_title,
      family = "FiraSans",
      size = 4.5,
      lineheight = 0.95
    )
}

# Run ---------------------------------------------------------------------

change_data <- load_data() |> build_change_data()

flag_data <- build_flag_data(unique(change_data$country))
lvls <- levels(filter(change_data, direction == "After birth")$country_name)
flag_data <- mutate(
  flag_data,
  country_name = factor(country_name, levels = lvls)
)

build_plot(change_data, flag_data) |>
  add_annotations(change_data) |>
  ggsave(
    filename = OUTPUT,
    device = ragg::agg_png,
    width = 14,
    height = 16,
    dpi = 340,
    bg = "#fff"
  )

message("Saved: ", OUTPUT)
