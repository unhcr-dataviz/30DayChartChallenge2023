# load package
library(tidyverse)
library(circlize)

# load data
pop_2022 <-
  readr::read_csv(here::here("11_circular", "population.csv"),
                skip = 14) |>
  janitor::clean_names() |>
  rename(coo_name = country_of_origin,
         coo_code = country_of_origin_iso,
         coa_name = country_of_asylum,
         coa_code = country_of_asylum_iso,
         ref = refugees_under_unhc_rs_mandate,
         asy = asylum_seekers,
         oip = other_people_in_need_of_international_protection) |>
  mutate(oip = as.double(if_else(oip == "-", NA, oip)))


region_hcr <-
  readr::read_csv(here::here("11_circular", "wrl_cntry_data_t_unhcr.csv")) |>
  select(iso3, region_pcode) |>
  mutate(region_pcode = if_else(region_pcode == "RBEHAGL" |
                                  region_pcode == "RBSA" |
                                  region_pcode == "RBWCA", "RBAF", region_pcode))


# group pop and region
pop_region_2022 <-
  pop_2022 |>
  left_join(region_hcr,
            by = c("coo_code" = "iso3")) |>
  left_join(region_hcr,
            by = c("coa_code" = "iso3")) |>
  rename(coo_region = region_pcode.x,
         coa_region = region_pcode.y) |>
  select(coo_region, coa_region, ref, asy, oip) |>
  pivot_longer(cols = ref:oip, names_to = "pop_type", values_to = "pop_num") |>
  group_by(coo_region, coa_region) |>
  summarise(pop_tot = sum(pop_num, na.rm = TRUE)) |>
  ungroup() |>
  filter(!(is.na(coo_region)))

pop_from <- pop_region_2022 |>
  mutate(coo_region = paste0("From", "\n", coo_region))

# parameters
circos.clear()
circos.par(start.degree = 90, clock.wise = FALSE)

# color palette
region_color = c(unhcrthemes::unhcr_pal(name = "pal_unhcr_region")[1],
                 unhcrthemes::unhcr_pal(name = "pal_unhcr_region")[4:7])

chord_color <- c("From\nRBAF" = region_color[1], "RBAF" = region_color[1],
                 "From\nRBA" = region_color[2], "RBA" = region_color[2],
                 "From\nRBAP" = region_color[3], "RBAP" = region_color[3],
                 "From\nRBE" = region_color[4], "RBE" = region_color[4],
                 "From\nRBMENA" = region_color[5], "RBMENA" = region_color[5])

chord_order <- c("From\nRBAF", "From\nRBA", "From\nRBAP", "From\nRBE", "From\nRBMENA",
                 "RBMENA", "RBE", "RBAP","RBA", "RBAF")

chordDiagram(pop_from,
             order = chord_order,
             grid.col = chord_color,
             direction.type = c("arrows"),
             transparency = 0.5,
             directional = 1,
             link.arr.type = "big.arrow",
             link.sort = TRUE,
             annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.05, 0.025),
             )

pop_region_2022 |>
  group_by(coo_region) |>
  summarise(pop = round(sum(pop_tot)/1000000, 1))

pop_region_2022 |>
  group_by(coa_region) |>
  summarise(pop = round(sum(pop_tot)/1000000, 1))
