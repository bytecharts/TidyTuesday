# Packages ---------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(here)

source("../theme/theme.R")

# Constants --------------------------------------------------------------

TITLE <- glue::glue(
  "How <span style='color:#F07178;'>male</span> and <span style='color:#2E86AB;'>female</span> penguins differ, trait by trait"
)

SUBTITLE <- glue::glue(
  "<b>Eudyptes pachyrhynchus</b> and <b>E. chrysocome</b> show the largest sex gaps; ",
  "<b>Eudyptes moseleyi</b> the smallest. ",
  "Of 18 recorded species, only <b>8</b> have measurements for both sexes.",
  "<br><span style='color:#2E86AB;'><b>F</b></span> = female · ",
  "<span style='color:#F07178;'><b>M</b></span> = male (sample size per sex)"
)

OUTPUT <- "penguins_diverging.png"

CAPTION <- caption_global(
  "AVONET dataset (Tobias et al. 2022)",
  "28",
  "Penguin Sex Differences"
)

# Data -------------------------------------------------------------------

penguins <- read.csv(
  "./data/many_penguins.csv",
  stringsAsFactors = FALSE
)

measure_cols <- c(
  "beak.length_culmen",
  "beak.length_nares",
  "beak.width",
  "beak.depth",
  "tarsus.length",
  "wing.length",
  "kipps.distance",
  "secondary1",
  "hand.wing.index",
  "tail.length"
)

measure_labels <- c(
  beak.length_culmen = "Beak length (culmen)",
  beak.length_nares = "Beak length (nares)",
  beak.width = "Beak width",
  beak.depth = "Beak depth",
  tarsus.length = "Tarsus length",
  wing.length = "Wing length",
  kipps.distance = "Kipp's distance",
  secondary1 = "Secondary 1",
  hand.wing.index = "Hand-wing index",
  tail.length = "Tail length"
)

# Prepare: mean per species / sex / measurement --------------------------
means <-
  penguins %>%
  filter(sex %in% c("F", "M")) %>%
  pivot_longer(
    all_of(measure_cols),
    names_to = "measure",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(species, shortname, genus, sex, measure) %>%
  summarise(value = mean(value), .groups = "drop")

# Difference: male mean minus female mean, as % of female mean ------------
diff <-
  means %>%
  pivot_wider(
    names_from = sex,
    values_from = value
  ) %>%
  filter(!is.na(F), !is.na(M)) %>%
  mutate(
    pct = (M - F) / F * 100,
    larger = factor(
      if_else(pct > 0, "Male", "Female"),
      levels = c("Female", "Male")
    )
  ) %>%
  mutate(measure = factor(measure, levels = measure_cols)) %>%
  mutate(measure_lab = recode(measure, !!!measure_labels))

# Female / male counts per species (for the row titles) ------------------
sex_counts <-
  penguins %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(shortname, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  rename(nF = F, nM = M)

# Plot -------------------------------------------------------------------
library(patchwork)
library(ggnewscale)

highlight_col <- "#F07178"

# Colour the end-of-bar labels by body region (bars themselves unchanged) --
group_map <- c(
  beak.length_culmen = "Beak",
  beak.length_nares = "Beak",
  beak.width = "Beak",
  beak.depth = "Beak",
  wing.length = "Wing",
  secondary1 = "Wing",
  hand.wing.index = "Wing",
  kipps.distance = "Wing",
  tarsus.length = "Tarsus",
  tail.length = "Other"
)

group_cols <- c(
  Beak = "#C77D3A",
  Wing = "#6C5CE7",
  Tarsus = "#2BB39A",
  Other = "#8A93A6"
)

# Common names for the 8 species with both sexes measured -----------------
common_names <- c(
  "A. forsteri" = "Emperor penguin",
  "A. patagonicus" = "King penguin",
  "E. chrysocome" = "Southern rockhopper penguin",
  "E. moseleyi" = "Northern rockhopper penguin",
  "E. pachyrhynchus" = "Fiordland penguin",
  "P. adeliae" = "Adélie penguin",
  "P. antarcticus" = "Chinstrap penguin",
  "P. papua" = "Gentoo penguin"
)

plot_list <- list()

sp_list <- unique(diff$shortname)
for (i in seq_along(sp_list)) {
  s <- sp_list[i]
  d_s <-
    diff %>%
    filter(shortname == s) %>%
    mutate(abs_pct = abs(pct)) %>%
    arrange(desc(pct)) %>%
    mutate(
      measure_lab = factor(measure_lab, levels = measure_lab),
      top3 = row_number() <= 3,
      lab_x = pct + if_else(pct >= 0, 6, -6),
      lab_h = if_else(pct >= 0, 0, 1),
      group = group_map[measure]
    )

  sc <- sex_counts %>% filter(shortname == s)
  common <- common_names[s]
  title_lab <- glue::glue(
    "<span style=\"font-size:14pt;font-weight:bold;\">{common}</span><br>",
    "<span style=\"font-size:11pt;\">{s}</span><br>",
    "<span style=\"color:#2E86AB;\">F</span>: ",
    "<span style=\"color:#2E86AB;\">{sc$nF}</span> / ",
    "<span style=\"color:#F07178;\">M</span>: ",
    "<span style=\"color:#F07178;\">{sc$nM}</span>"
  )

  p_s <-
    ggplot(d_s, aes(x = pct, y = measure_lab, fill = larger)) +
    geom_vline(xintercept = 0, colour = theme_fg, linewidth = 0.8) +
    geom_col(width = 0.4) +
    geom_text(
      aes(
        label = sprintf("%+.0f%%", pct),
        hjust = if_else(pct >= 0, -0.1, 1.1),
        colour = larger
      ),
      size = 3,
      family = "FiraCode"
    ) +
    scale_x_continuous(
      breaks = seq(-40, 40, 10),
      labels = function(x) paste0(x, "%"),
      limits = c(-72, 72),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_discrete(expand = expansion(add = 0.4)) +
    scale_fill_manual(
      values = c(Female = "#2E86AB", Male = "#F07178"),
      name = NULL,
      guide = "none"
    ) +
    new_scale_fill() +
    geom_label(
      aes(
        x = lab_x,
        y = measure_lab,
        label = measure_lab,
        hjust = lab_h,
        fill = group
      ),
      vjust = 0.5,
      colour = "white",
      size = 3,
      family = "FiraSans",
      label.size = NA,
      label.padding = unit(1.5, "mm"),
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = group_cols,
      name = "Body region"
    ) +
    scale_color_manual(
      values = c(Female = "#2E86AB", Male = "#F07178"),
      guide = "none"
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title_lab, x = NULL, y = NULL) +
    theme_base() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 9, color = theme_fg),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_textbox_simple(
        size = 14,
        face = "bold",
        color = theme_fg,
        family = "SpaceGrotesk",
        hjust = 0,
        margin = margin(b = 4),
        fill = "white",
        box.color = "white",
        width = unit(1, "npc")
      ),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 6)
    )

  plot_list[[s]] <- p_s
}

combined <-
  wrap_plots(plot_list, ncol = 1, guides = "collect") +
  plot_annotation(
    title = TITLE,
    subtitle = SUBTITLE,
    caption = CAPTION,
    theme = theme_base() +
      theme(
        plot.title = element_textbox_simple(
          family = theme_title_family,
          color = primary,
          face = "bold",
          size = 38,
          hjust = 0,
          width = unit(1, "npc"),
          padding = margin(5, 1, 5, 1),
          margin = margin(b = 6),
          fill = "white",
          box.color = "white"
        ),
        plot.subtitle = element_textbox_simple(
          size = 22,
          family = "FiraSans",
          hjust = 0,
          color = theme_muted,
          margin = margin(t = 5, b = 0),
          width = unit(1, "npc"),
          lineheight = 1.4,
          fill = "white",
          box.color = "white"
        ),
        plot.caption = element_markdown(
          size = 16,
          color = theme_muted,
          hjust = 0,
          family = theme_caption_family,
          lineheight = 1.5,
          margin = margin(t = 15)
        ),
        legend.position = "right",
        plot.margin = margin(55, 30, 30, 30)
      )
  )

ggsave(
  OUTPUT,
  combined,
  width = 14,
  height = 26,
  dpi = 300,
  bg = night_owlish_light$bg
)
