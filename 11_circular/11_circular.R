# load package
library(tidyverse)

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
  select(iso3, region_pcode)

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
  filter(!(is.na(coo_region)))
