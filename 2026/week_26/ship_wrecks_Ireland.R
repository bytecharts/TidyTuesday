# Packages ---------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggpattern)
library(lubridate)
library(here)

source("../theme/theme.R")
source("../utils/utils.R")

# Constants --------------------------------------------------------------

TITLE <- glue::glue(
  "<span style='color:#F07178;'>9 February 1861</span>: Ireland's Deadliest Day at Sea"
)


SUBTITLE_MAIN <- glue::glue(
  "Shipwrecks cluster around the winter months. ",
  "January, February, November, and December dominate the 19<sup>th</sup> Century."
)

SUBTITLE_CAL <- glue::glue(
  "Every panel compresses a century into a single calendar year. ",
  "Each cell represents how often that day appears in the historical shipwreck record."
)

OUTPUT <- "ship_wrecks_Ireland_calendar.png"

CAPTION <- caption_global(
  "Wreck Inventory of Ireland, rnli.org/about-us/our-history/timeline/1861-whitby-lifeboat-disaster",
  "26",
  "Ireland Shipwreck Calendar"
)

AQUA_BG <- alpha("#2E86AB", 0.3)

# Data -------------------------------------------------------------------

wrecks <- read.csv(
  "./data/wreck_inventory.csv",
  stringsAsFactors = FALSE
)

wrecks$date <- as.Date(wrecks$date)

# Prepare ---------------------------------------------------------------

df_prep <-
  wrecks %>%
  filter(
    !is.na(date),
    !is.na(year),
    year >= 1601
  ) %>%
  mutate(
    century = factor(
      paste0(floor((year - 1) / 100) + 1, "th Century"),
      levels = paste0(17:21, "th Century")
    ),
    calendar_date = make_date(
      2000,
      month(date),
      day(date)
    )
  ) %>%
  count(century, calendar_date, name = "wrecks") %>%
  group_by(century) %>%
  complete(
    calendar_date = seq(
      as.Date("2000-01-01"),
      as.Date("2000-12-31"),
      by = "day"
    ),
    fill = list(wrecks = 0)
  ) %>%
  ungroup() %>%
  mutate(
    month = month(calendar_date, label = TRUE, abbr = TRUE),
    week = isoweek(calendar_date),
    week = case_when(
      week >= 52 & month == "Jan" ~ 0,
      week == 1 & month == "Dec" ~ 53,
      TRUE ~ week
    ),
    weekday = wday(
      calendar_date,
      week_start = 1,
      label = TRUE,
      abbr = TRUE
    )
  )

# Days with 100+ wrekcs
highlight <- df_prep %>%
  filter(wrecks >= 100)


# Month polygons --------------------------------------------------------

month_polygons <- function(days) {
  days <-
    days %>%
    distinct(calendar_date, month, week, weekday)

  months_info <-
    days %>%
    group_by(month) %>%
    summarise(
      wd_first = as.numeric(weekday[which.min(calendar_date)]),
      wd_last = as.numeric(weekday[which.max(calendar_date)]),
      wc_first = week[which.min(calendar_date)],
      wc_last = week[which.max(calendar_date)],
      .groups = "drop"
    )

  months_info %>%
    rowwise() %>%
    reframe(
      month = month,
      x = c(
        wc_first - .5,
        wc_first - .5,
        wc_last - .5,
        wc_last - .5,
        wc_last + .5,
        wc_last + .5,
        wc_first + .5,
        wc_first + .5,
        wc_first - .5
      ),
      y = c(
        wd_first - .5,
        7.5,
        7.5,
        wd_last + .5,
        wd_last + .5,
        .5,
        .5,
        wd_first - .5,
        wd_first - .5
      )
    )
}

df_polygons <-
  df_prep %>%
  group_by(century) %>%
  group_modify(~ month_polygons(.x)) %>%
  ungroup()

month_labels <-
  df_prep %>%
  group_by(month) %>%
  summarise(
    x = mean(range(week)),
    .groups = "drop"
  )


century_labels <- c(
  "17th Century" = "17<sup>th</sup> Century",
  "18th Century" = "18<sup>th</sup> Century",
  "19th Century" = "<span style='color:#F07178;'>19<sup>th</sup> Century</span><br><span style='font-size:15pt;color:#F07178;'>9 February<br>161 wrecks</span>",
  "20th Century" = "20<sup>th</sup> Century",
  "21th Century" = "21<sup>st</sup> Century"
)

# Plot ------------------------------------------------------------------

p_calendar <-
  ggplot(
    df_prep,
    aes(
      x = week,
      y = as.numeric(weekday),
      fill = wrecks
    )
  ) +
  geom_tile(
    aes(fill = wrecks),
    colour = "white",
    linewidth = .2
  ) +

  geom_tile(
    data = highlight,
    aes(week, as.numeric(weekday)),
    inherit.aes = FALSE,
    fill = NA,
    colour = "#fff",
    linewidth = 1.25
  ) +
  geom_tile(
    data = highlight,
    aes(week, as.numeric(weekday)),
    inherit.aes = FALSE,
    fill = NA,
    colour = night_owlish_light$fg,
    linewidth = 0.5
  ) +

  geom_text(
    data = highlight,
    aes(
      week,
      as.numeric(weekday),
      label = "9"
    ),
    colour = "white",
    fontface = "bold",
    size = 4.5
  ) +

  geom_path(
    data = df_polygons,
    aes(x, y, group = interaction(century, month)),
    inherit.aes = FALSE,
    colour = night_owlish_light$bg,
    linewidth = 2.4,
    linejoin = "round"
  ) +

  geom_path(
    data = df_polygons,
    aes(x, y, group = interaction(century, month)),
    inherit.aes = FALSE,
    colour = night_owlish_light$fg_soft,
    linewidth = 0.8,
    linejoin = "round"
  ) +
  scale_x_continuous(
    breaks = month_labels$x,
    labels = month_labels$month,
    expand = expansion(mult = c(.01, .03))
  ) +
  scale_y_reverse(
    breaks = 1:7,
    labels = levels(df_prep$weekday)
  ) +
  coord_equal() +
  facet_wrap(
    ~century,
    ncol = 1,
    strip.position = "right",
    labeller = labeller(century = century_labels)
  ) +
  scale_fill_gradientn(
    colours = c(
      "#DCEBF2",
      "#4A6984",
      "#2E86AB",
      "#FFB454",
      "#F07178"
    ),
    limits = c(0, 40),
    oob = scales::squish,
    breaks = c(0, 10, 20, 30, 40),
    labels = c("0", "10", "20", "30", "40+")
  ) +
  labs(
    title = TITLE,
    subtitle = SUBTITLE_CAL,
    caption = CAPTION,
    x = NULL,
    y = NULL
  ) +
  theme_base() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 16),
    strip.text.y.right = element_markdown(
      angle = 0,
      face = "bold",
      hjust = 0,
      size = 20,
      lineheight = 1.1
    ),

    plot.margin = margin(10, 40, 10, 40),
    legend.position = "bottom",
    legend.text = element_text(
      size = 14,
      color = theme_muted,
      margin = margin(t = 10)
    ),
    plot.subtitle = element_textbox_simple(
      fill = "#dff3fb",
      size = 24,
      margin = margin(30, 40, 30, 40)
    ),
    legend.title = element_text(
      margin = margin(b = 10)
    )
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = unit(120, "mm"),
      barheight = unit(5, "mm")
    )
  ) +
  labs(
    title = NULL,
    caption = NULL
  )

# Bar Plot ------------------------------------------------------------------
feb9 <- wrecks %>%
  filter(
    year >= 1801,
    year <= 1900,
    month(date) == 2,
    day(date) == 9
  ) %>%
  count(year)

p_bar <-
  ggplot(feb9, aes(year, n)) +
  geom_col(
    fill = "#F07178",
    width = 0.9
  ) +
  geom_text(
    data = subset(feb9, n == max(n)),
    aes(label = n),
    vjust = -0.4,
    colour = "#F07178",
    size = 8
  ) +
  annotate(
    "curve",
    x = 1873,
    y = max(feb9$n) * 1.02,
    xend = 1861,
    yend = max(feb9$n) * 0.82,
    curvature = -0.25,
    linewidth = 0.8,
    colour = "#5A6270",
    arrow = arrow(
      type = "closed",
      length = unit(2.5, "mm")
    )
  ) +
  annotate(
    "text",
    x = 1882,
    y = max(feb9$n) * 1.25,
    label = "This spike corresponds to \nthe Great Gale of 9 February 1861.",
    colour = "#F07178",
    hjust = 0.5,
    size = 5.5
  ) +
  scale_x_continuous(
    breaks = seq(1800, 1900, 20)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Shipwrecks on 9 February by year",
    x = "19th Century",
    y = "Number of Wrecks
    on Feb 9"
  ) +
  theme_base() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(60, 20, 0, 20),
    axis.title.x = element_text(
      size = 18,
      color = theme_fg,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 18,
      color = theme_fg,
      margin = margin(r = 15)
    ),
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )


# Final Plot ------------------------------------------------------------------
library(patchwork)

final_plot <-
  p_bar /
  p_calendar +
  plot_layout(
    heights = c(1, 4)
  ) +
  plot_annotation(
    title = TITLE,
    subtitle = SUBTITLE_MAIN,
    caption = CAPTION,
    theme = theme_base() +
      theme(
        plot.title = element_textbox_simple(
          fill = "#dff3fb",
          size = 42,
          margin = margin(b = 10, t = 10),
        ),
        plot.subtitle = element_textbox_simple(
          fill = "#dff3fb",
          size = 24
        )
      )
  )

ggsave(
  OUTPUT,
  final_plot,
  width = 16,
  height = 20,
  dpi = 300,
  bg = "#dff3fb"
)
