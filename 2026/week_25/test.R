library(tidyverse)
library(viridis)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------

df <- read_csv("./data/papal_encyclicals.csv")

popes <-
  df |>
  mutate(
    pontificate_start = as.Date(pontificate_start),
    pontificate_end = coalesce(
      as.Date(pontificate_end),
      as.Date("2026-12-31")
    )
  ) |>
  group_by(
    pope,
    pontificate_start,
    pontificate_end
  ) |>
  summarise(
    encyclicals = n(),
    .groups = "drop"
  ) |>
  mutate(
    duration = as.numeric(pontificate_end - pontificate_start) / 365.25,

    enc_per_year = encyclicals / duration
  ) |>
  arrange(duration)

p <-
  ggplot(
    popes,
    aes(y = reorder(pope, duration))
  ) +

  geom_col(
    aes(
      x = duration,
      fill = enc_per_year
    ),
    width = .65
  ) +

  geom_point(
    aes(
      x = duration,
      size = encyclicals
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 1
  ) +

  geom_text(
    aes(
      x = duration,
      label = encyclicals
    ),
    hjust = -0.6,
    size = 3.5
  ) +

  scale_fill_viridis(
    option = "C",
    name = "Encyclicals / year"
  ) +

  scale_size(
    range = c(3, 12),
    name = "Total encyclicals"
  ) +

  scale_x_continuous(
    expand = expansion(mult = c(0, .08))
  ) +

  labs(
    title = "Pontificate Duration and Encyclical Output",
    subtitle = "Bar = Years as Pope • Colour = Encyclicals per Year • Circle = Total Encyclicals",
    x = "Years as Pope",
    y = NULL
  ) +

  theme_minimal(base_size = 15) +

  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Display
p

# Save
ggsave(
  filename = "pontificate_duration_encyclicals.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
