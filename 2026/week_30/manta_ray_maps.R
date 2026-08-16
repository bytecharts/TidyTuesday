# Packages ---------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(sf)
library(rnaturalearth)
library(patchwork)
library(cowplot)

source("../theme/theme.R")
source("../utils/utils.R")

# Palette: #154D71 #1C6EA4 #33A1E0 #FFF9AF

# Constants --------------------------------------------------------------

TITLE <- "Manta Ray Observations in Australia"
SUBTITLE <- "May \u2013 August: Patterns in recorded observations only.<br>Human observations peak in June; machine observations peak in July."
OUTPUT <- "manta_ray_australia.png"
CAPTION <- caption_global(
  "ecotourism, occurrences.csv",
  "30",
  "Manta Ray Winter Observations"
)

# Data -------------------------------------------------------------------

raw <- read.csv("./data/occurrences.csv", stringsAsFactors = FALSE)

manta_all <- raw %>%
  filter(
    organism_name == "Manta ray",
    record_type %in% c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION")
  )

# Trend line data ---------------------------------------------------------

trend_data <- manta_all %>%
  count(month, record_type, name = "n") %>%
  mutate(
    month_label = factor(
      month,
      levels = 1:12,
      labels = c(
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec"
      )
    ),
    obs_type = factor(
      record_type,
      levels = c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION"),
      labels = c("Human", "Machine")
    )
  )

# Map data (May-Aug) ------------------------------------------------------

manta_maps <- manta_all %>% filter(month %in% c(5, 6, 7, 8))

manta_agg <- manta_maps %>%
  mutate(rounded_lat = round(obs_lat, 2), rounded_lon = round(obs_lon, 2)) %>%
  count(month, record_type, rounded_lat, rounded_lon, name = "n") %>%
  mutate(
    month_label = factor(
      month,
      levels = c(5, 6, 7, 8),
      labels = c("MAY", "JUNE", "JULY", "AUGUST")
    ),
    obs_type = factor(
      record_type,
      levels = c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION"),
      labels = c("Human", "Machine")
    )
  )

manta_sf <- st_as_sf(
  manta_agg,
  coords = c("rounded_lon", "rounded_lat"),
  crs = 4326
)

# Basemap -----------------------------------------------------------------

au_outline <- ne_countries(
  scale = "medium",
  country = "Australia",
  returnclass = "sf"
)
au_states <- st_read("./data/au_states.geojson", quiet = TRUE)

# Extents -----------------------------------------------------------------

MAIN_EXTENT <- c(
  xmin = 143,
  xmax = 155,
  ymin = -27,
  ymax = -15
)

INSET_EXTENT <- c(
  xmin = 112,
  xmax = 118,
  ymin = -26,
  ymax = -19
)

# Ocean polygons -----------------------------------------------------------

make_ocean <- function(ext) {
  vals <- unname(ext[c("xmin", "ymin", "xmax", "ymax")])

  xmin <- vals[1]
  ymin <- vals[2]
  xmax <- vals[3]
  ymax <- vals[4]

  coords <- matrix(
    c(
      xmin,
      ymin,
      xmax,
      ymin,
      xmax,
      ymax,
      xmin,
      ymax,
      xmin,
      ymin
    ),
    ncol = 2,
    byrow = TRUE
  )

  st_sf(
    geometry = st_sfc(
      st_polygon(list(coords)),
      crs = 4326
    )
  )
}

main_ocean <- make_ocean(MAIN_EXTENT)
inset_ocean <- make_ocean(INSET_EXTENT)

main_ocean <- make_ocean(MAIN_EXTENT)
inset_ocean <- make_ocean(INSET_EXTENT)

# Ocean layers (palette) --------------------------------------------------

ocean_layers <- tibble(
  fill = c("#154D71", "#1C6EA4", "#1C6EA4", "#33A1E0", "#33A1E0"),
  alpha = c(0.70, 0.55, 0.40, 0.30, 0.20)
)

# Annotation labels -------------------------------------------------------

month_annotations <- tibble(
  month_label = factor(
    c("MAY", "JUNE", "JULY", "AUGUST"),
    levels = c("MAY", "JUNE", "JULY", "AUGUST")
  ),
  peak_label = c("", "Human peak", "Machine peak", ""),
  human_count = c(
    "Human: 13 obs",
    "Human: 96 obs",
    "Human: 31 obs",
    "Human: 23 obs"
  ),
  machine_count = c(
    "Machine: 27 obs",
    "Machine: 136 obs",
    "Machine: 245 obs",
    "Machine: 110 obs"
  )
)

# Location labels ---------------------------------------------------------

loc_stats <- manta_maps %>%
  mutate(rounded_lat = round(obs_lat, 2), rounded_lon = round(obs_lon, 2)) %>%
  count(rounded_lon, rounded_lat, record_type, name = "n") %>%
  group_by(rounded_lon, rounded_lat) %>%
  summarise(
    total = sum(n),
    dominant = record_type[which.max(n)],
    .groups = "drop"
  )

location_labels <- tibble(
  location = c(
    "Town of 1770",
    "Capricorn Coast",
    "Gladstone",
    "Whitsundays",
    "Cairns"
  ),
  obs_lon = c(152.71, 151.97, 152.38, 148.82, 146.50),
  obs_lat = c(-24.11, -23.44, -23.91, -20.25, -18.70)
) %>%
  left_join(
    loc_stats,
    by = c("obs_lon" = "rounded_lon", "obs_lat" = "rounded_lat")
  ) %>%
  mutate(
    total = replace_na(total, 1),
    dominant = replace_na(dominant, "HUMAN_OBSERVATION"),
    label_size = scales::rescale(total, to = c(3.0, 5.5)),
    label_color = ifelse(dominant == "HUMAN_OBSERVATION", "#F07178", "#1C6EA4"),
    label_lon = obs_lon - 2.0,
    label_lat = obs_lat + 1.0
  )

# Trend line plot ---------------------------------------------------------

p_trend <- ggplot(
  trend_data,
  aes(x = month, y = n, color = obs_type, group = obs_type)
) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(shape = obs_type), size = 4) +
  scale_color_manual(values = c("Human" = "#F07178", "Machine" = "#1C6EA4")) +
  scale_shape_manual(values = c("Human" = 16, "Machine" = 15)) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  annotate(
    "rect",
    xmin = 4.5,
    xmax = 8.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#154D71",
    alpha = 0.06
  ) +
  annotate(
    "text",
    x = 6.5,
    y = max(trend_data$n) * 1.08,
    label = "May\u2013Aug",
    size = 3.5,
    color = "#154D71",
    fontface = "italic",
    family = "FiraSans"
  ) +
  annotate(
    "text",
    x = 6,
    y = 260,
    label = "Human peak",
    size = 3.5,
    color = "#F07178",
    hjust = 0.5,
    family = "FiraSans",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = 7,
    y = 260,
    label = "Machine peak",
    size = 3.5,
    color = "#1C6EA4",
    hjust = 0.5,
    family = "FiraSans",
    fontface = "italic"
  ) +
  labs(x = NULL, y = "Number of observations") +
  theme_base() +
  theme(
    panel.grid.major.y = element_line(color = "#E8ECF2", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 13, color = primary, family = "FiraSans"),
    axis.text.y = element_text(size = 12, color = primary, family = "FiraSans"),
    axis.title.y = element_text(
      size = 14,
      color = primary,
      family = "FiraSans",
      margin = margin(r = 10)
    ),
    legend.position = "none",
    plot.margin = margin(40, 20, 5, 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Build inset (Exmouth) ---------------------------------------------------

build_inset <- function(month_data) {
  coords <- st_coordinates(month_data)
  month_data$lon <- coords[, 1]

  p <- ggplot()
  for (i in seq_len(nrow(ocean_layers))) {
    p <- p +
      geom_sf(
        data = inset_ocean,
        fill = ocean_layers$fill[i],
        color = NA,
        alpha = ocean_layers$alpha[i]
      )
  }
  p +
    geom_sf(
      data = main_ocean,
      fill = "#1C6EA4",
      color = NA
    ) +
    geom_sf(
      data = au_outline,
      fill = "#FFF9AF",
      color = "#154D71",
      linewidth = 0.5
    ) +
    geom_sf(
      data = au_states,
      fill = NA,
      color = "#33A1E0",
      linewidth = 0.2
    ) +
    scale_size_area(max_size = 5, limits = c(1, NA), guide = "none") +
    annotate(
      "text",
      x = 114.5,
      y = -14.5,
      label = "Exmouth",
      size = 2.8,
      color = "#154D71",
      hjust = 0,
      family = "FiraSans",
      fontface = "italic"
    ) +
    annotate(
      "segment",
      x = 114.1,
      xend = 114.1,
      y = -21.8,
      yend = -20.5,
      color = "#154D71",
      linewidth = 0.2,
      linetype = "dotted"
    ) +
    coord_sf(
      xlim = unname(MAIN_EXTENT[c("xmin", "xmax")]),
      ylim = unname(MAIN_EXTENT[c("ymin", "ymax")]),
      crs = st_crs(4326),
      datum = NA,
      expand = FALSE,
      clip = "on"
    ) +

    theme_base() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(
        fill = "#1C6EA4",
        color = "#154D71",
        linewidth = 0.8
      ),
      plot.background = element_rect(fill = "#1C6EA4", color = NA)
    )
}

# Build one map panel with inset ------------------------------------------

build_panel <- function(month_data, annotation) {
  p <- ggplot()
  for (i in seq_len(nrow(ocean_layers))) {
    p <- p +
      geom_sf(
        data = main_ocean,
        fill = ocean_layers$fill[i],
        color = NA,
        alpha = ocean_layers$alpha[i]
      )
  }

  p <- p +
    geom_sf(data = au_states, fill = NA, color = "#33A1E0", linewidth = 0.2) +
    geom_sf(
      data = au_outline,
      fill = "#FFF9AF",
      color = "#154D71",
      linewidth = 0.5
    ) +
    annotate(
      "segment",
      x = location_labels$label_lon,
      y = location_labels$label_lat,
      xend = location_labels$obs_lon,
      yend = location_labels$obs_lat,
      color = "#154D71",
      linewidth = 0.3,
      linetype = "dotted"
    ) +
    annotate(
      "text",
      x = location_labels$label_lon,
      y = location_labels$label_lat,
      label = location_labels$location,
      size = location_labels$label_size,
      color = location_labels$label_color,
      hjust = 1,
      family = "FiraSans",
      fontface = "italic"
    ) +
    geom_sf(
      data = month_data %>% filter(obs_type == "Machine"),
      aes(size = n),
      shape = 22,
      fill = "#1C6EA4",
      color = "#154D71",
      alpha = 0.88,
      stroke = 0.4
    ) +
    geom_sf(
      data = month_data %>% filter(obs_type == "Human"),
      aes(size = n),
      shape = 21,
      fill = "#F07178",
      color = "#A93226",
      alpha = 0.92,
      stroke = 0.4
    ) +
    scale_size_area(max_size = 12, limits = c(1, NA), guide = "none") +
    coord_sf(
      xlim = c(MAIN_EXTENT["xmin"], MAIN_EXTENT["xmax"]),
      ylim = c(MAIN_EXTENT["ymin"], MAIN_EXTENT["ymax"]),
      crs = 4326,
      datum = NA,
      clip = "on"
    ) +

    theme_base() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(
        fill = "#1C6EA4",
        color = NA
      ),

      plot.background = element_rect(
        fill = "#1C6EA4",
        color = NA
      )
    )

  inset <- build_inset(month_data)
  p + inset_element(inset, 0.46, 0.38, 0.38, 0.45, align_to = "panel")
}

# Build all panels --------------------------------------------------------

map_panels <- list()
for (i in seq_len(nrow(month_annotations))) {
  ann <- month_annotations[i, ]
  m_data <- manta_sf %>% filter(month_label == ann$month_label)
  map_panels[[i]] <- build_panel(m_data, ann)
}

# Assemble with cowplot ---------------------------------------------------

maps_grid <- plot_grid(
  map_panels[[1]],
  map_panels[[2]],
  map_panels[[3]],
  map_panels[[4]],
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

combined <- plot_grid(
  p_trend,
  maps_grid,
  ncol = 1,
  rel_heights = c(0.4, 1)
)

combined <- ggdraw(combined) +
  draw_label(
    TITLE,
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 42,
    fontface = "bold",
    color = primary
  ) +
  draw_label(
    "May - August: Patterns in recorded observations only. Human observations peak in June; machine observations peak in July.",
    x = 0.01,
    y = 0.965,
    hjust = 0,
    vjust = 1,
    size = 16,
    color = theme_muted
  ) +
  draw_label(
    "Viz: Byte Charts | Source: ecotourism, occurrences.csv | TidyTuesday 2026 - Week 30 - Manta Ray Winter Observations",
    x = 0.01,
    y = 0.01,
    hjust = 0,
    vjust = 0,
    size = 10,
    color = theme_muted
  )

# Save -------------------------------------------------------------------

ggsave(OUTPUT, combined, width = 16, height = 20, dpi = 300, bg = "#E8F4FD")
