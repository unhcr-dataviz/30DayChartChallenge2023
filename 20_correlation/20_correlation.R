# Load packages
library(tidyverse)

# setup
path <- here::here("20_correlation")

# load data
new_sol <- read_csv(here::here("20_correlation", "ref_new_vs_solution.csv")) |>
  janitor::clean_names() |>
  rename(new = new_refugee_recognitions_and_refugee_like_increases,
         sol = refugee_solutions_naturalisation_resettlement_and_returns)

new_sol |>
  ggplot(aes(x = new,
             y = sol)) +
  geom_segment(aes(xend = c(tail(new, n = -1), NA),
                   yend = c(tail(sol, n = -1), NA)),
               color = "lightpink",
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_point(color = "blue") +
  scale_x_continuous(limits = c(0, 3.5e6)) +
  scale_y_continuous(limits = c(0, 3.5e6)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_label(aes(label = year))
