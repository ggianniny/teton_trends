library(tidyverse)
library(brms)
library(tidybayes)

source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

temp_clean <- left_join(temp_clean,
                        source_info)

temp_clean %>% 
  filter(is.na(source))

temp_aug <- temp_clean %>% 
  mutate(month = month(date1),
         year = year(date1))%>%
  select(temp_c, year, month, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source),
         !year == 2023,
         month == 8) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_s = (temp_c - mean(temp_c)) / sd(temp_c))

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


# b_snow = b_year + b_year:snow
m %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow)%>%
  ggplot(aes(x = snow)) +
  stat_halfeye()

# snow intercept: b_Intercept + b_sourcesnowmelt
m %>%
  spread_draws(b_Intercept,
               `b_sourcesnowmelt`) %>%
  rename(year_snow = `b_sourcesnowmelt`) %>%
  mutate(snow_intercept = b_Intercept + year_snow)%>%
  ggplot(aes(x = snow_intercept)) +
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
            alpha = 0.25) +
  labs(title = "Snow")

m %>%
  spread_draws(b_Intercept, 
               b_sourcesub_ice,
               b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(sub = b_year_s + year_sub) %>%
  filter(.iteration < 100) %>%
  expand_grid(year_s = seq(-1.987, 1.722, length.out = 5)) %>%
  mutate(temp_pred = 
           sub * year_s + b_Intercept + b_sourcesub_ice) %>%
  ggplot(aes(x = year_s, 
             y = temp_pred,
             group = interaction(.draw, .iteration))) +
  geom_line(linewidth = 0.25, 
            alpha = 0.25) +
  labs(title = "Sub Ice")



# probability sub_ice = positive
m %>%
  spread_draws(b_Intercept, 
               b_sourcesub_ice,
               b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub > 0) %>%
  summarize(mean(sub_pos))

m %>%
  spread_draws(b_Intercept, 
               b_sourcesub_ice,
               b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub < 0) %>%
  summarize(mean(sub_pos))

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
            alpha = 0.25) +
  labs(title = "Should be glacier, but looks like overall effect of temp_s?")


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



c_eff <- conditional_effects(brm1)
c_eff$year_s %>% head()
plot(density(c_eff$year_s$estimate__))
c_eff$source %>% head()
c_eff$`year_s:source` %>% head()

mean_year <- mean(unique(temp_aug$year))
sd_year <- sd(unique(temp_aug$year))
mean_temp <- mean(unique(temp_aug$temp_c))
sd_temp <- sd(unique(temp_aug$temp_c))

c_eff$`year_s:source` %>%
  as_tibble() %>%
  mutate(year = (year_s*sd_year)+mean_year,
         est_temp = (estimate__*sd_temp)+mean_temp,
         est_lo = (lower__*sd_temp)+mean_temp,
         est_high = (upper__*sd_temp)+mean_temp) %>%
  ggplot(aes(x = year,
             y = est_temp,
             color = source,
             fill = source)) +
  geom_ribbon(aes(ymin = est_lo,
                  ymax = est_high), alpha = 0.25) +
  geom_line() +
  geom_pointrange(data = c_eff$source %>%
                    mutate(
                      year = (year_s*sd_year)+mean_year,
                      est_temp = (estimate__*sd_temp)+mean_temp,
                      est_lo = (lower__*sd_temp)+mean_temp,
                      est_high = (upper__*sd_temp)+mean_temp),
                  aes(x = year,
                      y = est_temp,
                      ymin = est_lo,
                      ymax = est_high)) +
  facet_grid(~source) +
  ggthemes::scale_fill_colorblind()+
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  scale_x_continuous(labels = round)
