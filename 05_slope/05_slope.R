# Load packages
library(ForcedDisplacementStat)
library(tidyverse)
library(ggrepel)
library(scales)
library(svglite)

# Set data filter
disp_filter <- c("REF", "ASY", "OIP")
year_filter <- c(2012, 2022)


# Load data
df_disp <- end_year_population_totals_long |>
  filter(Year %in% year_filter,
         Population.type %in% disp_filter) |>
  group_by(CountryOriginCode, CountryOriginName, Year) |>
  summarise(displ_tot = sum(Value, na.rm = TRUE)) |>
  ungroup() |>
  rename(iso_coo = CountryOriginCode, name_coo = CountryOriginName,
         year = Year)

df_disp_wide <- df_disp |>
  pivot_wider(names_from = year, values_from = displ_tot) |>
  janitor::clean_names() |>
  replace_na(list(x2012 = 0, x2022 = 0)) |>
  mutate(diff = x2022 - x2012)


down_filter <- df_disp_wide |>
  filter(iso_coo != "UKN") |>
  slice_min(order_by = diff, n = 5) |>
  pull(iso_coo)

up_filter <- df_disp_wide |>
  slice_max(order_by = diff, n = 5) |>
  pull(iso_coo)

df_top <- df_disp |>
  filter(iso_coo %in% down_filter | iso_coo %in% up_filter) |>
  mutate(highlight = if_else(iso_coo %in% down_filter, "down", "up"))


plot <- df_top |>
  ggplot(aes(x = year,
             y = displ_tot,
             group = iso_coo)) +
  geom_line(aes(color = highlight),
            alpha = .5) +
  geom_text(aes(label = iso_coo),
                  nudge_x = .5) +
  geom_point(aes(color = highlight)) +
  scale_x_continuous(breaks = c(2012, 2022)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("#0072bc", "#EF4A60")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(t = 30, r = 80, b = 30, l = 30))


ggsave("slope.svg", device = svglite,
       width = 7.5, height = 9)


df_top |>
  pivot_wider(names_from = "year",
              values_from = "displ_tot") |>
  mutate(label = paste(name_coo,
                       "- from",
                       if_else(`2012` > 1e6,
                               paste0(round(`2012` / 1e6, 1), "M"),
                               paste0(round(`2012` / 1e3, 0), "k")),
                       "to",
                       if_else(`2022` > 1e6,
                               paste0(round(`2022` / 1e6, 1), "M"),
                               paste0(round(`2022` / 1e3, 0), "k")))) |>
  select(label)



df_disp |>
  group_by(year) |>
  summarise(tot_disp = sum(displ_tot, na.rm = TRUE))
