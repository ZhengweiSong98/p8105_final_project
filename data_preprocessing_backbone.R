## This is a processing of raw data


library(tidyverse)
library(lubridate)

knitr::opts_chunk$set(
    fig.width = 6,
    fig.asp = .6,
    out.width = "90%",
    dpi = 300
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

age_df = read_csv("data/age.csv")
demo_df = read_csv("data/demo.csv")


## dataset of age_df
age =
    age_df %>%
    janitor::clean_names() %>%
    filter(demographic_category == "Age Group") %>%
    select(report_date,
            demographic_value,
            total_cases,
            deaths,
            percent_deaths,
            percent_of_ca_population) %>%
    drop_na() %>%
    rename("age_group" = demographic_value,
           "date" = report_date) %>%
    filter(!age_group %in% c("missing", "Missing","Total"))

race =
    age_df %>%
    janitor::clean_names() %>%
    filter(demographic_category == "Race Ethnicity") %>%
    select(report_date,
           demographic_value,
           total_cases,
           deaths,
           percent_deaths,
           percent_of_ca_population) %>%
    drop_na() %>%
    rename("age_group" = demographic_value,
           "date" = report_date)

gender =
    age_df %>%
    janitor::clean_names() %>%
    filter(demographic_category == "Gender") %>%
    select(report_date,
           demographic_value,
           total_cases,
           deaths,
           percent_deaths,
           percent_of_ca_population) %>%
    drop_na() %>%
    rename("gender" = demographic_value,
           "date" = report_date)


## dataset of post_stat
stat_12 = read_csv("data/12_plus.csv")
stat_16 = read_csv("data/16_plus.csv")
post_stat = read_csv("data/post_stat.csv")

stat_12 = stat_12[-(1:6),]
stat_12 =
    stat_12 %>%
    arrange(date, decreasing = F) %>%
    select(-c(area_type, area))



stat_16 = stat_16[-(1:6),]
stat_16 =
    stat_16 %>%
    janitor::clean_names() %>%
    arrange(date, decreasing = F) %>%
    select(-c(area_type, area))

post_stat = post_stat[-(1:6),]
post_stat =
    post_stat %>%
    janitor::clean_names() %>%
    arrange(date, decreasing = F) %>%
    select(-c(area_type, area))

## dataset of demo

demo_df = read_csv("data/demo.csv")

demo =
    demo_df %>%
    filter(area_type == "County") %>%
    select(-c(area_type, total_tests, positive_tests, reported_cases, reported_deaths, reported_tests)) %>%
    rename("county_name" = area)


