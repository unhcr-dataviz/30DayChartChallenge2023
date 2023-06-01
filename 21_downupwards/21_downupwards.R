# #30DayChartChallenge | April 2023 - Day 21 | theme: down/upwards
# Data source: UNHCR Global Trends 2021

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggbraid)
library(ggtext)
library(unhcrthemes)
library(ragg)
library(magick)


# setup -------------------------------------------------------------------

showtext::showtext_opts(dpi = 300)

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

# camcorder
# camcorder::gg_record(
#   dir = here::here("temp-plot"),
#   #scale = 2,
#   device = agg_png,
#   width = 2400,
#   height = 1350,
#   units = "px"
# )

# annotation
note <- tibble(
  x = ymd("2004-06-30"),
  y = 2.28e6,
  label = "<span style='color:#0072bc;'>**New displacements**</span> outpaced the available <span style='color:#00B398;'>**solutions**</span> during the last decade"
)

# plot
plot <-
  ggplot() +
  geom_braid(data = df_wide,
             aes(x = year, ymin = new, ymax = sol, fill = new < sol),
             alpha = 0.3) +
  geom_line(data = df_long,
            aes(x = year, y = value, color = name)) +
  geom_textbox(data = note, aes(x = x, y = y, label = label),
               hjust = 0, vjust = 1, width = grid::unit(0.25, "npc"),
               fill = NA, box.color = NA, size = 10/2.84) +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                     limits = c(0, NA),
                     expand = expansion(c(0, 0.02))) +
  scale_color_manual(values = c("#0072bc", "#00B398")) +
  scale_fill_manual(values = c("#0072bc", "#00B398")) +
  labs(title = "How do <span style='color:#0072bc;'>refugee recognitions</span><sup style='font-size: 12px;'>a</sup> compare with available <span style='color:#00B398;'>solutions</span><sup style='font-size: 12px;'>b</sup>?",
       caption = "<sup>a </sup>Refugee recognition refers to the legal process through which an individual is granted refugee status<br>by a country's government. - <sup>b </sup>Refugee solutions (returns, resettlement and naturalization)<br>Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 12, legend = FALSE, axis_title = FALSE, grid = "Yy", rel_tiny = 6/9) +
  theme(plot.title = element_markdown(),
        plot.caption = element_markdown(lineheight = 1.2),
        plot.background = element_rect(fill = "white"))


# save --------------------------------------------------------------------

path <- here::here("21_downupwards", "/")

# base plot
ggsave(paste0(path, "plot.png"), plot = plot, device = agg_png, dpi = 300,
       width = 2400, height = 1350, units = "px")

# load logo and chart
chart <- image_read(paste0(path, "plot.png"))
logo <- image_read_svg("https://raw.githubusercontent.com/vidonne/unhcrdesign/master/inst/resources/logo/unhcr_logo_blue.svg",
                       height = 100)

# put chart and logo together
plot_wlogo <- chart |>
  image_composite(logo, offset = "+1930+1200")

image_write(plot_wlogo, path = paste0(path, "21_downupwards.png"), format = "png")
