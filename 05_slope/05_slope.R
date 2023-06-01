# #30DayChartChallenge | April 2023 - Day 05 | theme: slope
# Data source: Refugee Data Finder

# load packages -----------------------------------------------------------

library(tidyverse)
library(nansen)
library(ggtext)
library(fontawesome)
library(unhcrthemes)
library(ragg)
library(magick)


# setup -------------------------------------------------------------------

showtext::showtext_opts(dpi = 300)
blue <- unhcrthemes::unhcr_pal(n = 1, name = "pal_blue")
red <- unhcrthemes::unhcr_pal(n = 1, name = "pal_red")

# load data ---------------------------------------------------------------

df_raw <- nansen::population |>
  filter(year %in% c(2012, 2022)) |>
  select(year, coo_name, coo_iso, refugees, asylum_seekers, oip)

# wrangle -----------------------------------------------------------------

df_long <- df_raw |>
  pivot_longer(cols = refugees:oip) |>
  group_by(year, coo_name, coo_iso) |>
  summarise(displacment = sum(value, na.rm = TRUE)) |>
  ungroup()

df_wide <- df_long |>
  pivot_wider(names_from = year,
              values_from = displacment) |>
  mutate(diff = `2022` - `2012`)

down_filter <- df_wide |>
  filter(!is.na(coo_iso)) |>
  slice_min(order_by = diff, n = 5) |>
  pull(coo_iso)

up_filter <- df_wide |>
  slice_max(order_by = diff, n = 5) |>
  pull(coo_iso)

df_plot <- df_long |>
  filter(coo_iso %in% down_filter | coo_iso %in% up_filter) |>
  mutate(highlight = if_else(coo_iso %in% down_filter, "down", "up"))

# plot --------------------------------------------------------------------

# camcorder
camcorder::gg_record(
  dir = here::here("temp-plot"),
  #scale = 2,
  device = agg_png,
  width = 2400,
  height = 1350,
  units = "px"
)

# annotation
note <- tibble(
  x = 2012,
  y = 7.1e6,
  label = paste0("In <b style='color:", blue, ";'>10 years</b> the total number of refugees<sup>a </sup> <b style='color:", red, ";'>more than tripled</b> from a little bit over <b style='color:", blue, ";'>11 million</b> in 2012<br>to almost <b style='color:", blue, ";'>37 million</b> in mid-2022")
)

annotation <- df_plot |>
  pivot_wider(names_from = "year",
              values_from = "displacment") |>
  mutate(x = 2022, y = `2022`,
    coo_label = case_when(
      coo_iso == "SRB" ~ "Serbia<sup>b</sup>",
      coo_iso == "VEN" ~ "Venezuela",
      TRUE ~ coo_name),
    label = paste0("<b>", coo_label, " - </b>From <b style='color:",
                   if_else(highlight == "down", red, blue), ";'>",
                   if_else(`2012` > 1e6,
                           paste0(round(`2012` / 1e6, 1), "M"),
                           paste0(round(`2012` / 1e3, 0), "k")),
                   "</b> to <b style='color:",
                   if_else(highlight == "down", red, blue), ";'>",
                   if_else(`2022` > 1e6,
                           paste0(round(`2022` / 1e6, 1), "M"),
                           paste0(round(`2022` / 1e3, 0), "k")),
                   "</b>"),
    move = case_when(
      coo_iso == "VNM" ~ 1,
      coo_iso == "COL" ~ .38,
      TRUE ~ .5
    )) |>
  select(x, y, label, move)


# plot
plot <-
  ggplot(data = df_plot,
       aes(x = year,
           y = displacment)) +
  geom_line(aes(group = coo_iso,
                color = highlight),
            linewidth = .75, alpha = .75) +
  geom_point(aes(group = coo_iso,
                 color = highlight),
             size = 2) +
  geom_textbox(data = note, aes(x = x, y = y, label = label),
               hjust = 0, vjust = 1, width = grid::unit(0.75, "npc"),
               fill = NA, box.color = NA, size = 10/2.84, family = "Lato", color = "grey15") +
  geom_textbox(data = annotation,
               aes(x = x, y = y, label = label, vjust = move),
               hjust = 0,  fill = NA, box.color = NA, size = 8/2.84,
               family = "Lato", color = "grey15") +
  coord_cartesian(clip = "off") +
  scale_color_unhcr_d(nmax = 9, order = c(9, 1)) +
  scale_x_continuous(breaks = c(2012, 2022),
                     position = "top",
                     expand = expansion(c(0.01, 0.01))) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                     breaks = seq(0, 7e6, 1e6)) +
  labs(title = paste0("Five biggest <span style='color:", blue, ";'>increase \u2191</span> and <span style='color:", red, ";'>decrease 	\u2193</span> in refugee population of country<br>of origin between 2012 and 2022"),
       caption = "<sup>a </sup>Refugee population includes refugees, asylum-seekers and other people in need of international protection<br><sup>b</sup>Serbia and Kosovo: S/RES/1244 (1999) (SRB)<br>Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 12, legend = FALSE, rel_tiny = 6/9, rel_small = 7/9,
              axis_title = FALSE, axis = FALSE,  grid = "YX") +
  theme(panel.grid.major.x = element_line(color = "grey10", linewidth = 0.5),
        plot.title = element_markdown(lineheight = 1.1),
        plot.caption = element_markdown(lineheight = 1.2),
        plot.margin = margin(12, 140, 12, 12),
        plot.background = element_rect(fill = "white"))



# save --------------------------------------------------------------------

path <- here::here("05_slope", "/")

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

image_write(plot_wlogo, path = paste0(path, "05_slope.png"), format = "png")

