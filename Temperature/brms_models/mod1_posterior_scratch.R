library(tidyverse)
library(brms)
library(tidybayes)

m <- readRDS("Temperature/brms_models/fit_temp_yearXsource_rand_site_year.rds")

get_variables(m)

m %>%
  spread_draws(`b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  ggplot(aes(x = year_snow)) +
  stat_halfeye()


m %>%
  spread_draws(`b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(b_pos = year_snow > 0) %>%
  summarize(mean(b_pos))

m %>%
  spread_draws(`b_year_s:sourcesnowmelt`) %>%
  rename(
    year_snow = `b_year_s:sourcesnowmelt`) %>%
  summarise(min(year_snow))


m %>%
  spread_draws(`b_year_s:sourcesnowmelt`) %>%
  rename(
    year_snow = `b_year_s:sourcesnowmelt`) %>%
  median_qi()



m %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow)%>%
  ggplot(aes(x = snow)) +
  stat_halfeye()

m %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow,
         snow_pos = snow > 0) %>%
  summarize(mean(snow_pos))

m %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow) %>%
  filter(snow<0)

get_variables(m)
m %>%
  spread_draws(b_Intercept, 
               b_sourcesnowmelt,
               b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow) %>%
  filter(.iteration < 100) %>%
  expand_grid(year_s = seq(-1.987, 1.722, length.out = 5)) %>%
  mutate(temp_pred = 
           snow * year_s + b_Intercept + b_sourcesnowmelt) %>%
  ggplot(aes(x = year_s, 
             y = temp_pred,
             group = interaction(.draw, .iteration))) +
  geom_line(linewidth = 0.25, 
            alpha = 0.25)




m %>%
  spread_draws(Intercept,
               sigma, 
               b_Intercept, 
               b_year_s) %>%
  filter(.iteration <= 100) %>%
  expand_grid(year_s = seq(-1.987, 1.722, length.out = 5)) %>%
  mutate(temp_pred = 
           year_s * b_year_s + b_Intercept) %>%
  ggplot(aes(x = year_s, 
             y = temp_pred,
             group = interaction(.draw, .iteration))) +
  geom_line(linewidth = 0.25, 
            alpha = 0.25)


plot(m)
get_variables(m)
m %>%
  spread_draws(Intercept)

m %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`,
               `b_year_s:sourcesub_ice`
               ) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`,
         year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(glacier = b_year_s,
         snow = b_year_s + year_snow,
         sub = b_year_s + year_sub)%>%
  select(glacier, year_snow, year_sub) %>%
  pivot_longer(1:3) %>%
  ggplot(aes(x = value,
             y = name)) +
  stat_halfeye()
