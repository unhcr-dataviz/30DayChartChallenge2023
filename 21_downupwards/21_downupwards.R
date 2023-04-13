# #30DayChartChallenge | April 2023 - Day 21 | theme: down/upwards
# Data source: UNHCR Global Trends 2021

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(systemfonts)


# prep --------------------------------------------------------------------

col_new <- "#0072bc"
col_sol <- "#00B398"

theme_set(theme_minimal(base_size = 18, base_family = "Lato"))
theme_update(
  legend.position = "none",
  plot.margin = margin(30, 30, 30, 30)
)


# load data ---------------------------------------------------------------


