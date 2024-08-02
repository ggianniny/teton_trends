# random slopes

library(tidyverse)
library(brms)
library(tidybayes)


source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

temp_clean <- left_join(temp_clean,
                        source_info)
temp_aug <- temp_clean %>% 
  mutate(month = month(date1),
         year = year(date1),
         day = day(date1))%>%
  select(temp_c, year, month, day, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source),
         !year == 2023,
         month == 8,
         site != "silver_run") %>%
  mutate(year_s = (year - mean(year)) / sd(year))

temp_aug <- temp_aug %>%
  group_by(site, year, year_s, day, source) %>%
  summarize(t_max = max(temp_c),
            t_min = min(temp_c))

temp_aug <- temp_aug %>%
  ungroup() %>%
  mutate(max_s = (t_max - mean(t_max)) / sd(t_max),
         min_s = (t_min - mean(t_min)) / sd(t_min))


temp_aug %>%
  ggplot(aes(x = year_s,
             y = max_s, 
             color = site)) +
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(method = "lm", 
              inherit.aes = FALSE,
              color = "black",
              aes(x = year_s, y = max_s)) +
  facet_wrap(~source, scales = "free_y")

temp_aug %>%
  ggplot(aes(x = year_s,
             y = min_s, 
             color = site)) +
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(method = "lm", 
              inherit.aes = FALSE,
              color = "black",
              aes(x = year_s, y = min_s)) +
  facet_wrap(~source, scales = "free_y")



get_prior(max_s ~ year_s * source + (1 + year_s |site) +
            (1|year_s),
          data = temp_aug)


my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))


brm1 <- brm(max_s ~ year_s * source +
              (1 + year_s |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)
tictoc::tic()
brm1 <- update(brm1,
               chains = 4,
               cores = 4,
               iter = 2000)
tictoc::toc()

saveRDS(brm1, "Temperature/brms_models/fit_rand_slopes_daily_max.rds")

brm2 <- brm(min_s ~ year_s * source +
              (1 + year_s |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)
tictoc::tic()
brm2 <- update(brm2,
               chains = 4,
               cores = 4,
               iter = 2000)
tictoc::toc()
saveRDS(brm2, "Temperature/brms_models/fit_rand_slopes_daily_min.rds")


plot(brm1)

pp_check(brm1,
         type = "stat_grouped",
         group = "source")
pp_check(brm1,
         type = "stat_grouped",
         group = "year_s")

pp_check(brm2,
         type = "stat_grouped",
         group = "source")
pp_check(brm2,
         type = "stat_grouped",
         group = "year_s")

pp_check(brm1)
pp_check(brm1,
         type = "boxplot")
pp_check(brm1,
         type = "dens_overlay_grouped",
         group = "source")
pp_check(brm1,
         ndraws = 100,
         type = "scatter_avg_grouped",
         group = "source")

bayes_R2(object = brm1)
bayes_R2(object = brm2)

conditional_effects(brm1)
conditional_effects(brm2)




get_variables(brm1)
# glacier
brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = b_year_s > 0) %>%
  summarize(mean(year_pos))
# 40%  +
# sub ice
brm1 %>%
  spread_draws(b_Intercept, 
               b_sourcesub_ice,
               b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub > 0) %>%
  summarize(mean(sub_pos))
# 47% +

# Snow
brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow,
         snow_pos = snow > 0) %>%
  summarize(mean(snow_pos))
# 95% +


# calculate mean and sd of original data 
c_eff <- conditional_effects(brm1)
mean_year <- mean(unique(temp_aug$year))
sd_year <- sd(unique(temp_aug$year))
mean_max_temp <- mean(unique(temp_aug$t_max))
sd_max_temp <- sd(unique(temp_aug$t_max))

c_eff2 <- conditional_effects(brm2)
mean_min_temp <- mean(unique(temp_aug$t_min))
sd_min_temp <- sd(unique(temp_aug$t_min))

c_eff$`year_s:source` %>%
  as_tibble() %>%
  mutate(year = (year_s*sd_year)+mean_year,
         est_temp = (estimate__*sd_max_temp)+mean_max_temp,
         est_lo = (lower__*sd_max_temp)+mean_max_temp,
         est_high = (upper__*sd_max_temp)+mean_max_temp) %>%
  ggplot(aes(x = year,
             y = est_temp,
             color = source,
             fill = source)) +
  geom_ribbon(aes(ymin = est_lo,
                  ymax = est_high), 
              alpha = 0.25,
              color = NA) +
  geom_line() +
  geom_pointrange(data = c_eff$source %>%
                    mutate(
                      year = (year_s*sd_year)+mean_year,
                      est_temp = (estimate__*sd_temp)+mean_max_temp,
                      est_lo = (lower__*sd_max_temp)+mean_max_temp,
                      est_high = (upper__*sd_max_temp)+mean_max_temp),
                  aes(x = year,
                      y = est_temp,
                      ymin = est_lo,
                      ymax = est_high)) +
  facet_grid(~source) +
  ggthemes::scale_fill_colorblind()+
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  scale_x_continuous(labels = round,
                     #breaks = seq(2014, 2022, by = 1)
                     ) +
  scale_y_continuous(#breaks = seq(2.5, 18, by = 0.6)
    ) +
  labs(y = "Daily Maximum Temperature")

brm1
# glacier
fixef(brm1)
fixef(brm1)[2,1] * (sd_max_temp / sd_year)
# decrease: -0.06C per year
# snowmelt
(fixef(brm1)[2,1]+fixef(brm1)[5,1]) * (sd_max_temp / sd_year)
# increase of 0.413C per year
# Sub
(fixef(brm1)[2,1]+fixef(brm1)[6,1]) * (sd_max_temp / sd_year)
# decrease: -0.02C per year

brm1$data
# new year = 1.722[max year_s] + 0.529 [difference between year_s] = 2.251
pred_data <- expand_grid(source = unique(brm1$data$source),
                         year_s = c(1.722, 2.251))

# predict temperature in new year
# add epred draws for new year
preds <-add_epred_draws(
  brm1,
  newdata = pred_data,
  allow_new_levels = TRUE)

conditional_effects(brm1, effects = "year_s:source")

preds %>%
  filter(year_s == 2.251) %>%
  mutate(fill = .epred > 0) %>%
  mutate(.epred = (.epred/sd_max_temp)) %>%
  ggplot(aes(x = .epred,
             y = source,
         fill = after_stat(x > 0),
         color = source)) +
  scale_fill_manual(
    values = alpha(c("cadetblue3", "tomato2"), 0.35),
    labels = c("Decrease", "Increase")) +
  scale_color_manual(
    values= NatParksPalettes::natparks.pals("Triglav")
      ) +
  stat_halfeye() +
  theme_bw() +
  labs(x = "Temperature change, Celsius",
       title = "Predicted Maximum Temperature Change in 2024") +
  guides(fill=guide_legend(title="Prediction"))


# minimum temps

c_eff2$`year_s:source` %>%
  as_tibble() %>%
  mutate(year = (year_s*sd_year)+mean_year,
         est_temp = (estimate__*sd_min_temp)+mean_min_temp,
         est_lo = (lower__*sd_min_temp)+mean_min_temp,
         est_high = (upper__*sd_min_temp)+mean_min_temp) %>%
  select(year, est_temp, est_lo, est_high, source) %>%
  ggplot(aes(x = year,
             y = est_temp,
             color = source,
             fill = source)) +
  geom_ribbon(aes(ymin = est_lo,
                  ymax = est_high), 
              alpha = 0.25,
              color = NA) +
  geom_line() +
  geom_pointrange(data = c_eff2$source %>%
                    mutate(
                      year = (year_s*sd_year)+mean_year,
                      est_temp = (estimate__*sd_temp)+mean_min_temp,
                      est_lo = (lower__*sd_min_temp)+mean_min_temp,
                      est_high = (upper__*sd_min_temp)+mean_min_temp),
                  aes(x = year,
                      y = est_temp,
                      ymin = est_lo,
                      ymax = est_high)) +
  facet_grid(~source) +
  ggthemes::scale_fill_colorblind()+
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  scale_x_continuous(labels = round,
                     #breaks = seq(2014, 2022, by = 1)
  ) +
  scale_y_continuous(#breaks = seq(2.5, 18, by = 0.6)
  ) +
  labs(y = "Daily Minimum Temperature")

brm2
# glacier
fixef(brm2)
fixef(brm2)[2,1] * (sd_max_temp / sd_year)
# 0.03C per year
# snowmelt
(fixef(brm2)[2,1]+fixef(brm2)[5,1]) * (sd_max_temp / sd_year)
# increase of 0.82C per year
# Sub
(fixef(brm2)[2,1]+fixef(brm2)[6,1]) * (sd_max_temp / sd_year)
# 0.05C per year

preds2 <-add_epred_draws(
  brm2,
  newdata = pred_data,
  allow_new_levels = TRUE)

preds2 %>%
  filter(year_s == 2.251) %>%
  mutate(fill = .epred > 0) %>%
  mutate(.epred = (.epred/sd_max_temp)) %>%
  ggplot(aes(x = .epred,
             y = source,
             fill = after_stat(x > 0),
             color = source)) +
  scale_fill_manual(
    values = alpha(c("cadetblue3", "tomato2"), 0.35),
    labels = c("Decrease", "Increase")) +
  scale_color_manual(
    values= NatParksPalettes::natparks.pals("Triglav")
  ) +
  stat_halfeye() +
  theme_bw() +
  labs(x = "Temperature change, Celsius",
       title = "Predicted Minimum Temperature Change in 2024") +
  guides(fill=guide_legend(title="Prediction"))
