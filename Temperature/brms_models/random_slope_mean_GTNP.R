# Random intercept and slope for August in GTNP 

library(tidyverse)
library(brms)
library(tidybayes)


source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

unique(temp_clean$site)
# All sites
# Skillet, Teton, Middle Teton, Cloudveil
# Mt St John (gusher), South Cascade, Alaska Basin, Wind Cave
# Grizzly, Painbrush, N Fork Teton, S Fork Teton

# non GTNP sites to remove
rm_sites <- c("windcave", "s_teton", "n_teton", "s_ak_basin", "silver_run")

temp_clean <- temp_clean |>
  mutate(month = month(date1),
         year = year(date1)) |>
  filter(!is.na(temp_c),
         month == 8,
         !site %in% rm_sites)

temp_clean <- left_join(temp_clean,
                        source_info)
temp_clean |>
  select(site, year) |> 
  distinct() #|> View()

temp_aug <- temp_clean %>% 
  select(temp_c, year, month, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source),
         month == 8) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_s = (temp_c - mean(temp_c)) / sd(temp_c))

temp_aug %>%
  ggplot(aes(x = year,
             y = temp_c, 
             color = site)) +
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(method = "lm", 
              inherit.aes = FALSE,
              color = "black",
              aes(x = year, y = temp_c)) +
  facet_wrap(~source, scales = "free_y")

get_prior(temp_s ~ year_s * source + (1 + year_s |site) + (1|year_s),
          data = temp_aug)


my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))

# fit "quick" model
# make sure it works, and avoid need to compile each time. 
brm1 <- brm(temp_s ~ year_s * source +
              (1 + year_s |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

# update the above model with more chains, cores, and iterations
tictoc::tic()
brm1 <- update(brm1,
               chains = 4,
               cores = 4,
               iter = 2000)
tictoc::toc()
# 2024-11-25 = ~01:20:00

# save the output
saveRDS(brm1, "Temperature/brms_models/fit_rand_slopes_aug_GTNP.rds")
# brm1 <- readRDS("Temperature/brms_models/fit_rand_slopes_aug_GTNP.rds")
plot(brm1)

pp_check(brm1,
         type = "stat_grouped",
         group = "source")
pp_check(brm1,
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

conditional_effects(brm1)

# model comparison without random slopes
# pretty sure this didn't work becasue of a difference in model structure or somethins?
# brm0 <- readRDS(("Temperature/brms_models/fit_temp_yearXsource_rand_site_year.rds"))
# 
# brm0$criteria
# 
# brm0 <- add_criterion(brm0, "waic")
# brm1 <- add_criterion(brm1, "waic")
# comp <- loo_compare(brm0, brm1, criterion = "waic")
# print(comp)


get_variables(brm1)
# glacier
brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = b_year_s > 0) %>%
  summarize(mean(year_pos))
# 69.2% +

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
# 74% +

# Snow
brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow,
         snow_pos = snow > 0) %>%
  summarize(mean(snow_pos))
# 99.4% +


# calculate mean and sd of original data 
c_eff <- conditional_effects(brm1)
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
                  ymax = est_high), 
              alpha = 0.25,
              color = NA) +
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
  scale_x_continuous(labels = round,
                     #breaks = seq(2014, 2022, by = 1)
                     ) +
  scale_y_continuous(#breaks = seq(2.5, 18, by = 0.6)
    ) +
  labs(y = "Temperature")

coef(brm1)
fixef(brm1)
# glacier
fixef(brm1)[2,1] * (sd_temp / sd_year)
# increase of 0.140

# snowmelt
(fixef(brm1)[2,1]+fixef(brm1)[5,1]) * (sd_temp / sd_year)
# increase of 0.999

# Sub
(fixef(brm1)[2,1]+fixef(brm1)[6,1]) * (sd_temp / sd_year)
# increase of 0.225


brm1$data
max(brm1$data$year_s)
# 2024 run max = 1.696
brm1$data$year_s %>% unique() %>% sort()
# 2024 run difference = 
1.6960715 - 1.2375859
# 0.4584856
1.6960715 + 0.4584856
# 2.154557
# UPDATE ????????
# new year = 1.722[max year_s] + 0.529 [difference between year_s] = 2.251
pred_data <- expand_grid(source = unique(brm1$data$source),
                         year_s = c(1.696, 2.155))

# predict temperature in new year
# add epred draws for new year
preds <-add_epred_draws(
  brm1,
  newdata = pred_data,
  allow_new_levels = TRUE)

conditional_effects(brm1, effects = "year_s:source")

preds %>%
  filter(year_s == 2.155) %>%
  mutate(fill = .epred > 0) %>%
  mutate(.epred = (.epred/sd_temp)) %>%
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
       title = "Predicted Temperature Change in 2025") +
  guides(fill=guide_legend(title="Prediction"))
