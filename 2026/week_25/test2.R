library(tidyverse)
library(lubridate)
library(viridis)
library(ragg)

# ==========================================================
# Load data
# ==========================================================

df <- read_csv("./data/papal_encyclicals.csv") |>
  mutate(
    pontificate_start = ymd(pontificate_start),
    pontificate_end = coalesce(
      ymd(pontificate_end),
      ymd("2026-12-31")
    )
  )

# ==========================================================
# Number of encyclicals published each year
# ==========================================================

yearly_counts <-
  df |>
  count(
    pope,
    year,
    name = "encyclicals"
  )

# ==========================================================
# Total encyclicals per pope
# ==========================================================

totals <-
  df |>
  count(
    pope,
    name = "total_encyclicals"
  )

# ==========================================================
# Expand every pontificate into individual years
# ==========================================================

pontificates <-
  df |>
  distinct(
    pope,
    pontificate_start,
    pontificate_end
  ) |>
  rowwise() |>
  mutate(
    year = list(
      seq(
        year(pontificate_start),
        year(pontificate_end)
      )
    )
  ) |>
  unnest(year) |>
  ungroup() |>
  left_join(
    yearly_counts,
    by = c("pope", "year")
  ) |>
  mutate(
    encyclicals = replace_na(encyclicals, 0)
  ) |>
  left_join(
    totals,
    by = "pope"
  )

# ==========================================================
# Bubble positions (last year of pontificate)
# ==========================================================

bubble_data <-
  pontificates |>
  group_by(pope) |>
  slice_max(year, n = 1) |>
  ungroup()

# ==========================================================
# Plot
# ==========================================================

p <-
  ggplot(
    pontificates,
    aes(
      x = year,
      y = forcats::fct_rev(
        forcats::fct_reorder(
          pope,
          pontificate_start
        )
      )
    )
  ) +

  geom_tile(
    aes(fill = encyclicals),
    width = 0.95,
    height = 0.75,
    colour = "white",
    linewidth = 0.25
  ) +

  geom_text(
    data = bubble_data,
    aes(
      x = year + 1.8,
      label = total_encyclicals
    ),
    hjust = 0,
    size = 3.5
  ) +

  scale_fill_gradientn(
    colours = c(
      "#F4F1EA", # parchment
      "#D8C9A3", # aged paper
      "#C9A227", # Vatican gold
      "#8C1D40" # cardinal red
    ),
    values = scales::rescale(c(0, 1, 2, 5)),
    name = "Encyclicals\npublished"
  ) +

  scale_size_area(
    max_size = 10,
    name = "Total\nEncyclicals"
  ) +

  scale_x_continuous(
    breaks = seq(1880, 2030, 10),
    expand = expansion(mult = c(0, 0.06))
  ) +

  labs(
    title = "Publication of Papal Encyclicals Across Each Pontificate",
    subtitle = "Each square represents one year of a pontificate. Colour indicates the number of encyclicals issued that year.",
    x = NULL,
    y = NULL
  ) +

  theme_minimal(base_size = 15) +

  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    legend.position = "right",
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12)
  )

# ==========================================================
# Save
# ==========================================================

agg_png(
  filename = "papal_encyclicals_timeline_heatmap.png",
  width = 3600,
  height = 2200,
  res = 300
)

print(p)

dev.off()
