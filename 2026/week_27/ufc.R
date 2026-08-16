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


OUTPUT <- "game_films_by_language.png"


CAPTION <- caption_global(
  "{fightr} R Package",
  "27",
  "UFC"
)
SUBCATEGORY_COLORS <- c(
  "English" = "#F07178",
  "Japanese" = "#2E86AB",
  "Mandarin/Cantonese" = "#7FDBCA"
)

# Data ------------------------------------------------------------------------

ufc_athletes <- read_csv('./data/ufc_athletes.csv')
ufc_fights <- read_csv('./data/ufc_fights.csv')
ufc_rankings_dataset <- read_csv('./data/ufc_rankings_dataset.csv')
ufcstats_data <- read_csv('./data/ufcstats_data.csv')
ultimate_ufc_dataset <- read_csv('./data/ultimate_ufc_dataset.csv')
