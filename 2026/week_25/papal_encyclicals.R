# Packages ---------------------------------------------------------------
library(tidyverse)
library(ggtext)

source("../theme/theme.R")
source("../utils/utils.R")

# Constants ---------------------------------------------------------------

TITLE <- glue::glue(
  "The rise of <span style='color:#F07178;'>English</span> video game films"
)

SUBTITLE <- glue::glue(
  "Among theatrical releases, <span style='color:#2E86AB;'>Japanese</span> films led for decades, ",
  "<span style='color:#F07178;'>English</span> films became the largest group in the 2020s and account for most upcoming releases, ",
  "while <span style='color:#7FDBCA;'>Mandarin/Cantonese</span> films emerged in the 2010s"
)


OUTPUT <- "papal_encyclicals.png"


CAPTION <- caption_global(
  "Vatican.ca",
  "25",
  "Papal Encyclicals"
)
SUBCATEGORY_COLORS <- c(
  "English" = "#F07178",
  "Japanese" = "#2E86AB",
  "Mandarin/Cantonese" = "#7FDBCA"
)

# Data ------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2026, week = 25)

encyclicals <- tuesdata$encyclicals
papal_encyclicals <- tuesdata$papal_encyclicals
scripture_references <- tuesdata$scripture_references



# Plot ------------------------------------------------------------------------

#game_films_plot <- ggplot(
#plot_data,
#aes(x = factor(decade_label), y = n, fill = subcategory)
#) +
#geom_col() +
#geom_col(
#color = "black",
#linewidth = 2,
#alpha = 0.95
#) +
#geom_text(
#aes(label = n),
#position = position_stack(vjust = 0.5),
#size = 8,
#color = "white",
#lineheight = 0.9,
#fontface = "bold"
#) +
#scale_fill_manual(values = SUBCATEGORY_COLORS) +
#scale_y_continuous(
#limits = c(0, 100),
#expand = expansion(mult = c(0, 0.05))
#) +
#labs(
#title = TITLE,
#subtitle = SUBTITLE,
#caption = CAPTION,
#x = "Release Decade",
#y = "Number of Films"
#) +
#theme_base() +
#theme(
#legend.position = "none",
#panel.grid.major = element_blank(),
#axis.title.x = element_text(hjust = 0.3)
#)

#ggsave(
#filename = OUTPUT,
#device = ragg::agg_png,
#width = 16,
#height = 20,
#dpi = 340,
#bg = "#fff"
#)
