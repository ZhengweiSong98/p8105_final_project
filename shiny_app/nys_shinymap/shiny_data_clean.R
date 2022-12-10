library(tidyverse)

source("/data_preprocessing_backbone.R")
labor_data = read_csv("./data/CA_Labor.csv") %>%
    janitor::clean_names() %>%
    dplyr::select(county, unemployment_rate_per_cent) %>%
    rename(unemployment=unemployment_rate_per_cent) %>%
    mutate(unemployment = case_when(
        unemployment <= 5 ~ "Normal",
        unemployment > 5 & unemployment < 10 ~ "High",
        unemployment >= 10 ~ "Extreme",
    ))

population_data = read_csv("./data/CA_Land_Area.csv") %>%
    janitor::clean_names() %>%
    dplyr::select(county, land_area_sq_mi)

geo_data = read_csv("./data/ca_boundaries.csv") %>%
    janitor::clean_names() %>%
    rename(county=name) %>%
    dplyr::select(county, intptlat, intptlon)

ca_joining_data = left_join(population_data, labor_data, by = c("county"))
ca_nonmedical_data = left_join(ca_joining_data, geo_data, by = c("county"))

demo_covid = demo %>%
    dplyr::select(date, county_name, cumulative_cases) %>%
    rename(county=county_name) %>%
    filter(!county %in% c("Out of state","Unknown"))

ca_shiny_data = left_join(demo_covid, ca_nonmedical_data, by = c("county"))
skimr::skim(ca_premodel_data)

write.csv(ca_shiny_data, file = "ca_shiny_data.csv")
