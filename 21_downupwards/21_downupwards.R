# #30DayChartChallenge | April 2023 - Day 21 | theme: down/upwards
# Data source: UNHCR Global Trends 2021

# load packages -----------------------------------------------------------

library(tidyverse)
library(svglite)
library(ggbraid)


# load data ---------------------------------------------------------------

df_raw <- readr::read_csv(here::here("data", "ref_new_vs_solution.csv")) |>
  janitor::clean_names()


# wrangle -----------------------------------------------------------------

df_long <- df_raw |>
  rename(new = new_refugee_recognitions_and_refugee_like_increases,
         sol = refugee_solutions_naturalisation_resettlement_and_returns) |>
  mutate(year = lubridate::ymd(year, truncated = 2L)) |>
  pivot_longer(cols = new:sol)

df_wide <- pivot_wider(df_long, names_from = name, values_from = value)

# plot --------------------------------------------------------------------

ggplot() +
  geom_braid(data = df_wide,
             aes(x = year, ymin = new, ymax = sol, fill = new < sol),
             alpha = 0.3) +
  geom_line(data = df_long,
            aes(x = year, y = value, color = name)) +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                     limits = c(0, NA),
                     expand = expansion(c(0.01,0.01))) +
  scale_color_manual(values = c("#0072bc", "#00B398")) +
  scale_fill_manual(values = c("#0072bc", "#00B398")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("solution.svg", device = svglite,
       width = 16, height = 9)
