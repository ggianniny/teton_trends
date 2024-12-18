# Random intercept and slope for August in GTNP 

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/GTNP_sites/all_sites/august"

brm1 <- readRDS("Temperature/brms_models/GTNP_sites/all_sites/august/fit_rand_slopes_aug_GTNP.rds")
plot(brm1)
pairs(brm1)

# posterior predictions ####
pp_check(brm1,
         type = "stat_grouped",
         group = "source")
# pp_check(brm1,
#          type = "stat_grouped",
#          group = "year_s")
# #pp_check(brm1)
# pp_check(brm1,
#          type = "boxplot")
# pp_check(brm1,
#          type = "dens_overlay_grouped",
#          group = "source")
# pp_check(brm1,
#          ndraws = 100,
#          type = "scatter_avg_grouped",
#          group = "source")

# model summaries ####
summary(brm1)
coef(brm1)$site[,,"year_s"]
coef(brm1)

bayes_R2(object = brm1) # 59%

conditional_effects(brm1)

# plot individual sites through time  
get_variables(brm1)

# calculate mean and sd of original data 
fit_data_orig <- readRDS(paste(write_dir, 
                               "/fit_data.rds",
                               sep = ""))
fit_data_orig |>
  select(site) |>
  distinct()

# add new years here
year_s_vector <- brm1$data$year_s %>% unique() %>% sort()
# difference between standardized years
# max year - penultimate max year
year_diff <- year_s_vector[length(year_s_vector)] - year_s_vector[(length(year_s_vector)-1)] 
year_diff
new_year <- year_s_vector[length(year_s_vector)] + year_diff
new_year

new_year_df <- brm1$data |>
  select(source, site) |>
  distinct() |>
  mutate(year_s = c(new_year,
                    new_year +year_diff)) # new years 

fit_data <- bind_rows(fit_data, new_year_df)

# simulate types of data you might collect
pred_plot <- add_predicted_draws(
  newdata = fit_data,
  brm1,
  allow_new_levels = TRUE) |>
  median_qi(.prediction) |>
  ggplot(aes(x = year_s, 
             y = .prediction,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(data = temp_aug,
             aes(x = year_s, 
                 y = temp_s,
                 color = source),
             inherit.aes = FALSE) +
  facet_wrap(source~site, 
             ncol = 2,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Predicted Temperatures in GTNP",
       x = "Year Z-score",
       y = "Predicted Temperature")

ggsave(plot = pred_plot,
       filename = paste(write_dir, "/Predicted_temps.png", 
                    sep = ""))
# this is what we found
fit_plot <- add_epred_draws(
  newdata = fit_data,
  brm1,
  allow_new_levels = TRUE) |>
  median_qi(.epred) |>
  ggplot(aes(x = year_s, 
             y = .epred,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  # geom_point(data = temp_aug,
  #            aes(x = year_s, 
  #                y = temp_s,
  #                color = source),
  #            inherit.aes = FALSE) +
  facet_wrap(source~site, 
             ncol = 2,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       x = "Year Z-score",
       y = "Fitted Temperature")
ggsave(plot = fit_plot,
       filename = paste(write_dir, "/fitted_temps.png", 
                        sep = ""))




# probability of positive slopes ####
get_variables(brm1)
# glacier
glac_pos <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = b_year_s > 0) %>%
  summarize(prob_pos = mean(year_pos)) |>
  mutate(source = "glacier")
# 69.2% +
# 2024-12-03 = (all 2024 data) = 92.2% +

# sub ice
sub_pos <- brm1 %>%
  spread_draws(#b_Intercept, 
               #b_sourcesub_ice,
               b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub > 0) %>%
  summarize(prob_pos = mean(sub_pos))|>
  mutate(source = "sub_ice")
# 74% +
# 2024-12-03 = (all 2024 data) = 80.6%+

# Snow
snow_pos <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(snow = b_year_s + year_snow,
         snow_pos = snow > 0) %>%
  summarize(prob_pos = mean(snow_pos)) |>
  mutate(source = "snow")
# 99.4% +
# 2024-12-03 = (all 2024 data) = 99.3%+

prob_pos_df <- bind_rows(snow_pos, glac_pos, sub_pos)
write_csv(prob_pos_df,
          file = paste(write_dir, "/prob_slope_positive.csv", 
                       sep = ""))

# distribution of source slopes ####
# halfeye plots of b_source
b_glacier <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(source = "glacier")

b_sub <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(b_year_s = b_year_s + year_sub) %>%
  select(-year_sub) |>
  mutate(source = "sub_ice")

b_snow <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowmelt`) %>%
  rename(year_snow = `b_year_s:sourcesnowmelt`) %>%
  mutate(b_year_s = b_year_s + year_snow) |>
  select(-year_snow) |>
  mutate(source = "snow")

b_year <- bind_rows(b_glacier, b_sub, b_snow)

source_slope_halfeye <- ggplot(b_year, 
       aes(y = source,
           x = b_year_s,
           fill = after_stat(x >0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "orangered1")) +
  theme_bw() +
  labs(title = "Posterior Distribution of Source Slope Coefficients",
       x = "Slope Coefficient",
       y = "Water Source") +
  guides(fill=guide_legend(title="Positive Slope"))
ggsave(plot = source_slope_halfeye,
       filename = paste(write_dir, "/source_slope_halfeye.png", 
                        sep = ""))

# source slopes ####
# calculate mean and sd of original data 
c_eff <- conditional_effects(brm1)
mean_year <- mean(unique(temp_aug$year))
sd_year <- sd(unique(temp_aug$year))
mean_temp <- mean(unique(temp_aug$temp_c))
sd_temp <- sd(unique(temp_aug$temp_c))

source_slopes_plot <- c_eff$`year_s:source` %>%
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
  labs(title = "Predicted Temperature by Source",
       y = "Temperature")
ggsave(plot = source_slopes_plot,
       filename = paste(write_dir, "/source_slopes.png", 
                        sep = ""))

# slope magnitude table ####
slope_mags <- data.frame(
  source = c("glacier", "snowmelt", "sub_ice"),
  magnitude = c(
    fixef(brm1)[2,1] * (sd_temp / sd_year),
    (fixef(brm1)[2,1]+fixef(brm1)[5,1]) * (sd_temp / sd_year),
    (fixef(brm1)[2,1]+fixef(brm1)[6,1]) * (sd_temp / sd_year)
  )
)

write_csv(slope_mags,
          file = paste(write_dir, "/slope_magnitudes.csv", 
                       sep = ""))

