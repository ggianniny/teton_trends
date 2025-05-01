# Teton annual analysis

library(tidyverse)
library(brms)
library(tidybayes)
library(forecast)


source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

source_info <- source_info %>%
  mutate(source = case_when(
    site == "cloudveil" ~ "sub_ice",
    .default = source)) |>
  mutate(source = case_when(
    source == "sub_ice" ~ "rock_glacier",
    source == "snowmelt" ~ "snowfield",
    .default = source))

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

temp_clean

temp_clean |>
  ggplot(aes(x = date1, 
             y = temp_c)) +
  geom_point() +
  facet_wrap(.~site)

temp_clean |>
  filter(site == "s_teton") |>
  ggplot(aes(x = date1, 
             y = temp_c)) +
  geom_point() +
  facet_wrap(~year, scales = "free")


temp_clean |>
  filter(site == "s_teton") |>
  ggplot(aes(x = date1, 
             y = temp_c)) +
  geom_point() +
  stat_smooth()
temp_clean |>
  filter(site == "s_teton") |>
  ggplot(aes(x = date1, 
             y = temp_c)) +
  #geom_point() +
  geom_line()

temp_clean |>
  filter(site == "s_teton") |>
  mutate(month = month(date1)) |>
  ggplot(aes(x = date_tm, 
             y = temp_c,
             color = month)) +
  geom_point()

# fitting splines
temp_clean |>
  filter(site == "s_teton",
         !is.na(temp_c)) |>
  pull(temp_c) |>
  ts(frequency = 24) |>
  decompose() |>
  autoplot()



# daily means
temp_clean |>
  filter(site == "s_teton") |>
  group_by(date1) |>
  summarize(m_daily = mean(temp_c, na.rm = TRUE)) |> 
  ggplot(aes(x = date1, 
             y = m_daily)) +
  geom_point() 




temp_clean |>
  filter(site == "s_teton",
         !is.na(temp_c)) |>
  group_by(date1) |>
  summarize(m_daily = mean(temp_c, na.rm = TRUE)) |> 
  pull(m_daily) |> 
  ts(frequency = 365) |>
  decompose() |>
  autoplot()


temp_clean |>
  filter(site == "s_teton",
         !is.na(temp_c),
         date1 > 2016-01-01 |
         date1 < 2023-12-31) |>
  group_by(date1) |>
  summarize(m_daily = mean(temp_c, na.rm = TRUE)) |> 
  pull(m_daily) |> 
  msts(seasonal.periods = c(7, 365)) |>
  mstl() |>
  autoplot()


# day of year plots
temp_clean |>
  filter(site == "s_teton" |
           site == "n_teton" |
           site == "grizzly",
         !is.na(temp_c)) |>
  group_by(site, date1) |>
  summarize(m_daily = mean(temp_c, na.rm = TRUE)) |> 
  mutate(day = yday(date1),
         year = year(date1)) |>
  ggplot(aes(x = day, 
             y = m_daily,
             color = year)) +
  geom_point(size = 2,
             alpha = 0.5) +
  facet_wrap(.~site) +
  theme_bw()


temp_clean |>
  filter(!is.na(temp_c)) |>
  group_by(site, year) |>
  summarize(m_annual = mean(temp_c, na.rm = TRUE)) |> 
  ggplot(aes(x = site, 
             y = m_annual,
             color = year)) +
  geom_point(size = 2) +
  #facet_wrap(.~site, scales = "free_x") +
  theme_bw()
