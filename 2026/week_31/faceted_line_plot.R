# Packages ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggimage)
library(magick)
library(rsvg)
library(patchwork)
library(terra)

# Use shared theme and caption utilities
source("../theme/theme.R")

# Flag helpers (same approach as week_22) -------------------------------
FLAG_DIR <- file.path(getwd(), "flags")

flag_cdn_code <- function(code) tolower(code)
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
    iso2 = codes,
    flag_path = file.path(dir, paste0(flag_cdn_code(codes), "_circle.png"))
  ) |>
    mutate(flag_path = map2_chr(flag_url(iso2), flag_path, clip_flag))
}

# Data file
data_file <- file.path("data", "basotho_wool.csv")
if (!file.exists(data_file)) {
  stop(paste0("data file '", data_file, "' not found"))
}

df <- readr::read_csv(data_file, show_col_types = FALSE)

# Rename reporter_desc to country
df <- df %>% rename(country = reporter_desc)

# Compute price_per_kg using primary_value and net_wgt
df <- df %>% mutate(price_per_kg = primary_value / net_wgt)

# Year from ref_year
df <- df %>% mutate(year = as.integer(ref_year))

# Clean and aggregate
df_clean <- df %>%
  select(country, year, price_per_kg) %>%
  filter(!is.na(country), !is.na(year), !is.na(price_per_kg))

# Top 4 countries by record count
chosen <- df_clean %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 4) %>%
  pull(country)

last_year <- 2023

df_plot <- df %>%
  filter(country %in% chosen, year == last_year) %>%
  select(country, primary_value, net_wgt) %>%
  filter(!is.na(primary_value), !is.na(net_wgt)) %>%
  group_by(country) %>%
  summarise(
    value = sum(primary_value, na.rm = TRUE),
    weight = sum(net_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_weight = weight / 1000,
    price_per_tonne = value / weight * 1000,
    iso2 = case_when(
      country == "China" ~ "cn",
      country == "India" ~ "in",
      country == "South Africa" ~ "za",
      country == "Uruguay" ~ "uy",
      TRUE ~ NA_character_
    ),
    flag_size = case_when(
      country == "China" ~ 0.125,
      country == "India" ~ 0.044,
      country == "South Africa" ~ 0.17,
      country == "Uruguay" ~ 0.054,
      TRUE ~ 0.05
    )
  ) %>%
  left_join(build_flag_data(.$iso2), by = "iso2")

# Maps with Lesotho highlighted ----------------------------------------
africa <- ne_countries(
  scale = "medium",
  continent = "Africa",
  returnclass = "sf"
)
lesotho <- africa %>% filter(name == "Lesotho")

# Terrain hillshade for the Lesotho map ---------------------------------
terrain_file <- file.path("terrain", "lesotho_terrain.tif")
if (file.exists(terrain_file)) {
  terrain_r <- terra::rast(terrain_file)[[1]]
  terrain_r <- terra::mask(terrain_r, terra::vect(lesotho))
  terrain_df <- as.data.frame(terrain_r, xy = TRUE, na.rm = TRUE)
  names(terrain_df)[3] <- "shade"

  # Elevation stats from terrarium tiles (elev = R*256 + G + B/256 - 32768)
  z <- 8L
  n_tiles <- 2^z
  world_m <- 20037508.342789244 * 2
  tile_m <- world_m / n_tiles
  elev_tiles <- list()
  for (tx in 146:149) {
    for (ty in 148:151) {
      ef <- sprintf("terrain/el_%d_%d.png", tx, ty)
      if (!file.exists(ef)) next
      er <- terra::rast(ef)[[1:3]]
      xmin_m <- -world_m / 2 + tx * tile_m
      ymax_m <- world_m / 2 - ty * tile_m
      terra::ext(er) <- terra::ext(xmin_m, xmin_m + tile_m, ymax_m - tile_m, ymax_m)
      terra::crs(er) <- "+init=epsg:3857"
      elev_tiles[[length(elev_tiles) + 1]] <- er
    }
  }
  if (length(elev_tiles) > 0) {
    elev_mosaic <- do.call(terra::mosaic, elev_tiles)
    elev_ll <- terra::project(elev_mosaic, "+init=epsg:4326")
    elev <- elev_ll[[1]] * 256 + elev_ll[[2]] + elev_ll[[3]] / 256 - 32768
    elev <- terra::mask(elev, terra::vect(lesotho))
    elev_vals <- as.numeric(terra::values(elev))
    elev_vals <- elev_vals[!is.na(elev_vals)]
    elev_avg <- round(mean(elev_vals))
    elev_max <- round(max(elev_vals))
  }
}

LESOTHO_FILL <- "#5CBFA8"

# 1. Lesotho zoom map (local region for context)
lesotho_region <- ne_countries(
  scale = "medium",
  country = c("Lesotho", "South Africa"),
  returnclass = "sf"
)

p_map_lesotho <- ggplot() +
  { if (file.exists(terrain_file)) {
      geom_raster(
        data = terrain_df,
        aes(x = x, y = y, fill = shade),
        interpolate = TRUE
      )
    }
  } +
  scale_fill_gradient(low = "#2A2F3A", high = "#FBFBFB", guide = "none") +
  geom_sf(
    data = lesotho_region,
    fill = NA,
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = lesotho,
    fill = alpha(LESOTHO_FILL, 0.4),
    color = "white",
    linewidth = 0.3
  ) +
  annotate(
    "text",
    x = 28,
    y = -30.7,
    label = paste0(
      "Avg. elevation: ", elev_avg, " m\n",
      "Max. elevation: ", elev_max, " m"
    ),
    size = 5,
    family = "FiraSans",
    color = theme_fg,
    hjust = 0.5,
    vjust = 1
  ) +
  coord_sf(xlim = c(26, 30), ylim = c(-31, -28)) +
  labs(
    title = "Lesotho",
    subtitle = "Most of Lesotho lies above 1,400 m, making it the world's only independent <span style='color:#5CBFA8;'>country</span> entirely <span style='color:#5CBFA8;'>above 1,000 m</span>.",
    x = NULL,
    y = NULL
  ) +
  theme_base() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_textbox_simple(
      family = theme_title_family,
      face = "bold",
      size = 24,
      color = LESOTHO_FILL,
      hjust = 0.5,
      fill = NA,
      box.color = NA,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = "FiraSans",
      size = 14,
      color = theme_muted,
      hjust = 0.5,
      fill = NA,
      box.color = NA,
      margin = margin(t = 10)
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

# 2. Africa map
p_map_africa <- ggplot() +
  geom_sf(
    data = africa,
    fill = "#CBD5E1",
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = lesotho,
    fill = LESOTHO_FILL,
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf_text(
    data = lesotho,
    aes(label = "Lesotho"),
    nudge_y = 3,
    size = 5,
    family = theme_title_family,
    fontface = "bold",
    color = LESOTHO_FILL
  ) +
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 37)) +
  labs(title = "Africa", x = NULL, y = NULL) +
  theme_base() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_textbox_simple(
      family = theme_title_family,
      face = "bold",
      size = 24,
      color = theme_fg,
      hjust = 0.5,
      fill = NA,
      box.color = NA
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

p_maps <- p_map_lesotho + p_map_africa

# Faceted line plot ------------------------------------------------------
TITLE <- "From the Highlands of Lesotho"
SUBTITLE <- "Merino sheep graze Lesotho's high altitude grasslands, producing fine wool exported primarily to South Africa, China, and India."
CAPTION <- caption_global(
  "UN Comtrade / Basotho Wool dataset | Freepik - Magnific.com. For the wool texture that is rendered in Figma.",
  "31",
  "Basotho Wool Trade"
)

p_lines <- ggplot(df_plot, aes(x = price_per_tonne, y = total_weight)) +
  geom_point(
    aes(size = total_weight),
    shape = 21,
    fill = "white",
    color = base_colors$neutral,
    alpha = 0.8,
    stroke = 1
  ) +
  geom_image(
    aes(image = flag_path, size = I(flag_size)),
    asp = 1
  ) +
  geom_label(
    aes(
      label = paste0(
        country,
        "\n",
        scales::comma(round(total_weight, 0)),
        " t | $",
        scales::comma(round(price_per_tonne, 0)),
        "/t"
      )
    ),
    vjust = 0.5,
    hjust = 0,
    nudge_x = 300,
    fill = "white",
    color = theme_fg,
    size = 5.25,
    family = "FiraSans",
    label.padding = unit(0.4, "lines"),
    linewidth = 0.3
  ) +
  scale_size_continuous(
    labels = scales::comma_format(),
    range = c(12, 48),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(prefix = "$"),
    limits = c(0, 12000),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    limits = c(0, 7000),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    x = "Price per metric tonne (USD) - 2023",
    y = "Total weight imported (metric tonnes) - 2023"
  ) +
  theme_base() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = alpha(theme_fg, 0.05)),
    plot.background = element_rect(fill = theme_bg, color = NA),
    panel.background = element_rect(fill = theme_bg, color = NA),
    legend.position = "none"
  )

# Combine with patchwork -------------------------------------------------
final_plot <- (p_maps / p_lines) +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = TITLE,
    subtitle = SUBTITLE,
    caption = CAPTION,
    theme = theme_base() +
      theme(
        plot.title = element_textbox_simple(
          fill = NA,
          box.color = NA,
          size = 42,
          margin = margin(t = 30, b = 12, l = 10, r = 10)
        ),
        plot.subtitle = element_textbox_simple(
          fill = NA,
          box.color = NA,
          size = 24,
          margin = margin(t = 8, b = 25, l = 10, r = 10)
        ),
        plot.margin = margin(30, 30, 30, 30)
      )
  )

# Save ------------------------------------------------------------------
out_file <- "lesotho_wool_map_and_lines.png"
ggsave(out_file, final_plot, width = 16, height = 20, dpi = 300, bg = theme_bg)

message("Saved: ", out_file)
