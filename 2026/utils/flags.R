library(ggimage)
library(magick)
library(rsvg)
library(countrycode)
library(dplyr)
library(purrr)

FLAG_DIR <- "flag_cache"

flag_cdn_code <- function(code) {
  ifelse(code == "UK", "gb", tolower(code))
}

flag_url <- function(code) {
  paste0("https://flagcdn.com/w80/", flag_cdn_code(code), ".png")
}

circle_mask_svg <- function(size = 80) {
  path <- tempfile(fileext = ".svg")

  writeLines(
    sprintf(
      "<svg xmlns='http://www.w3.org/2000/svg'
      width='%1$d'
      height='%1$d'
      viewBox='0 0 %1$d %1$d'>
      <circle cx='%2$d' cy='%2$d' r='%2$d' fill='white'/>
      </svg>",
      size,
      size / 2L
    ),
    path
  )

  path
}

clip_flag <- function(
  url,
  dest,
  size = 80,
  mask_path = circle_mask_svg(size)
) {
  flag <- image_read(url) |>
    image_resize(sprintf("%dx%d^", size, size)) |>
    image_crop(sprintf("%dx%d+0+0", size, size))

  mask <- rsvg_png(
    mask_path,
    width = size,
    height = size
  ) |>
    image_read()

  image_composite(
    flag,
    mask,
    operator = "copyopacity"
  ) |>
    image_write(dest, format = "png")

  dest
}

build_flag_data <- function(
  data,
  country_col = country,
  iso_col = iso3c,
  dir = FLAG_DIR
) {
  country_col <- rlang::enquo(country_col)
  iso_col <- rlang::enquo(iso_col)

  dir.create(
    dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  data |>
    distinct(!!country_col, !!iso_col) |>
    mutate(
      iso2 = countrycode(
        !!iso_col,
        "iso3c",
        "iso2c"
      ),
      flag_path = file.path(
        dir,
        paste0(flag_cdn_code(iso2), ".png")
      )
    ) |>
    mutate(
      flag_path = map2_chr(
        flag_url(iso2),
        flag_path,
        clip_flag
      )
    )
}
