# #30DayChartChallenge | April 2023 - Day 28 | theme: trend
# Data source: UNHCR Refugee Data Finder

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(svglite)



# load data ---------------------------------------------------------------

df_unhcr <- readr::read_csv(here::here("data", "pop_unhcr_2012_2022.csv"),
                            skip = 14) |>
  janitor::clean_names() |>
  select(year, ref_unhcr = refugees_under_unhc_rs_mandate,
         asy = asylum_seekers, oip = other_people_in_need_of_international_protection) |>
  mutate(oip = as.double(if_else(oip == "-", NA, oip)))

df_unrwa <- readr::read_csv(here::here("data", "ref_unrwa_2012_2022.csv"),
                            skip = 14) |>
  janitor::clean_names() |>
  select(year, ref_unrwa = total)

df_idmc <- readr::read_csv(here::here("data", "idp_idmc_2012_2022.csv"),
                           skip = 14) |>
  janitor::clean_names() |>
  select(year, idp_idmc = total)


# wrangle -----------------------------------------------------------------

df_pop <- bind_cols(df_unhcr, df_idmc, df_unrwa) |>
  select(-year...5, -year...7) |>
  rename(year = year...1) |>
  pivot_longer(cols = ref_unhcr:ref_unrwa,
               names_to = "pop_type",
               values_to = "pop_num") |>
  filter(!(is.na(pop_num)))


# plot --------------------------------------------------------------------

df_pop |>
  mutate(pop_type = factor(pop_type,
                           levels = c("oip", "asy", "ref_unrwa",
                                      "ref_unhcr", "idp_idmc")),
         year = lubridate::ymd(year, truncated = 2L)) |>
  ggplot(aes(x = year, y = pop_num, fill = pop_type)) +
  geom_col() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                     breaks = seq(0, 100e6, 20e6),
                     limits = c(0, NA),
                     expand = expansion(c(0.01,0.01))) +
  unhcrthemes::scale_fill_unhcr_d(palette = "pal_unhcr_poc", nmax = 9,
                                  order = c(6, 3, 2, 1, 4), labels = c("Others in need of international protection", "Asylum seekers", "Refugees under UNRWA's mandate", "Refugees under UNHCR's mandate", "IDPs")) +
  labs(title = "<b>Global forced displaced population</b> | 2012-2022",
       subtitle = "For the first time in 2022, forcibly displaced population overpass the <b style='color:#EF4A60'>100 million</b> mark",
       caption = "2022 IDPs figure are estimated using data available as of 9 June 2022\nSource: UNHCR Refugee Data Finder") +
  geom_hline(yintercept = 100e6, color = "#EF4A60", alpha = .8) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE)) +
  theme_minimal(base_family = "Lato", base_size = 18) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification = "left",
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(color = "black", size = rel(1.4)),
        plot.subtitle = element_markdown(color = "grey10"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, color = "grey40", size = rel(0.75)),
        axis.text = element_text(color = "grey10", size = rel(0.9)))


# save --------------------------------------------------------------------

ggsave("trends.svg", device = svglite,
       width = 16, height = 9)
