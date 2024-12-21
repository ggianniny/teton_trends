# Random intercept and slope for August in GTNP 

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/GTNP_sites/no_snow/august"

brm1 <- readRDS("Temperature/brms_models/GTNP_sites/no_snow/august/fit_rand_slopes_aug_GTNP.rds")
# plot(brm1)
# pairs(brm1)
brm1$data %>%
  select(site, source) |>
  distinct()

# posterior predictions ####
pp_check(brm1,
         type = "stat_grouped",
         group = "source")
pp_check(brm1,
         type = "stat_grouped",
         group = "year_s")
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
ranef(brm1)

bayes_R2(object = brm1)

conditional_effects(brm1)

# plot individual sites through time  ####
get_variables(brm1)

# calculate mean and sd of original data 
fit_data_orig <- readRDS(paste(write_dir, 
                               "/fit_data.rds",
                               sep = ""))
# fit_data_orig |>
#   select(year, year_s, site) |>
#   distinct() |>
#   View()


mean_year <- mean((fit_data_orig$year))
sd_year <- sd((fit_data_orig$year))
mean_temp <- mean((fit_data_orig$temp_c))
sd_temp <- sd((fit_data_orig$temp_c))

# simulate types of data you might collect
fit_data_obs_years <- brm1$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 8,
         day = 15,
         date = make_date(year, month, day))

# plot of predicting new data
pred_plot <- add_predicted_draws(
  newdata = fit_data_obs_years,
  brm1,
  allow_new_levels = TRUE) |>
  median_qi(.prediction) |>
  mutate(.prediction = (.prediction*sd_temp)+mean_temp,
         .lower = (.lower*sd_temp)+mean_temp,
         .upper = (.upper*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, 
             y = .prediction,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(data = brm1$data |>
               mutate(year = (year_s*sd_year)+mean_year,
                      month = 8,
                      day = 15,
                      date = make_date(year, month, day),
                      temp = (temp_s*sd_temp)+mean_temp),
             aes(x = date,
                 y = temp,
                 color = source),
             inherit.aes = FALSE) +
  facet_wrap(source~site, 
             ncol = 2,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Predicted Temperatures in GTNP",
       subtitle = "Ribbon shows predictions for new data",
       x = "Year",
       y = "Predicted Temperature")
pred_plot

ggsave(plot = pred_plot,
       filename = paste(write_dir, "/Predicted_temps.png", 
                    sep = ""))
# this is what we found
fit_plot <- add_epred_draws(
  newdata = fit_data_obs_years,
  brm1,
  allow_new_levels = TRUE) |>
  median_qi(.epred) |>
  mutate(.epred = (.epred*sd_temp)+mean_temp,
         .lower = (.lower*sd_temp)+mean_temp,
         .upper = (.upper*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, 
             y = .epred,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(data = brm1$data |>
               mutate(year = (year_s*sd_year)+mean_year,
                      month = 8,
                      day = 15,
                      date = make_date(year, month, day),
                      temp = (temp_s*sd_temp)+mean_temp),
             aes(x = date,
                 y = temp,
                 color = source),
             inherit.aes = FALSE) +
  facet_wrap(source~site, 
             ncol = 2,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       subtitle = "Ribbon is 95% CrI for mean temperature",
       x = "Year",
       y = "Fitted Temperature")
fit_plot
ggsave(plot = fit_plot,
       filename = paste(write_dir, "/fitted_temps.png", 
                        sep = ""))

# spaghetti of fits
fit_spag <- add_epred_draws(newdata = fit_data_obs_years,
                brm1,
                allow_new_levels = TRUE,
                ndraws = 100) |>
  mutate(.epred = (.epred*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, y = .epred, color = source)) +
  geom_line(aes(y = .epred, group = paste(site, .draw)), alpha = .2) +
  #geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(source~site, 
             ncol = 2,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       subtitle = "Spaghetti Plot, 100 draws",
       x = "Year",
       y = "Fitted Temperature")
fit_spag
ggsave(plot = fit_spag,
       filename = paste(write_dir, "/fitted_spaghetti.png", 
                        sep = ""))


# probability of positive slopes ####
get_variables(brm1)
# glacier
glac_pos <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = b_year_s > 0) %>%
  summarize(prob_pos = mean(year_pos)) |>
  mutate(source = "glacier")
# 95.2%

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
# 43.8 %


prob_pos_df <- bind_rows(glac_pos, sub_pos)
write_csv(prob_pos_df,
          file = paste(write_dir, "/prob_slope_positive.csv", 
                       sep = ""))

# distribution of source slopes ####
# halfeye plots of b_source
b_glacier <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(source = "glacier",
         b_year_s = b_year_s * (sd_temp / sd_year))

b_sub <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesub_ice`) %>%
  rename(year_sub = `b_year_s:sourcesub_ice`) %>%
  mutate(b_year_s = b_year_s + year_sub,
         b_year_s = b_year_s * (sd_temp / sd_year)) %>%
  select(-year_sub) |>
  mutate(source = "sub_ice")

b_year <- bind_rows(b_glacier, b_sub)

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
  guides(fill=guide_legend(title="Positive Slope")) +
  annotate("text", x = 0.15, y = 2.5,
           label = paste("P(Positive)",
                         round(sub_pos[1,1],2),
                         sep = " ")) +
  annotate("text", x = 0.2, y = 1.5,
           label = paste("P(Positive)",
                         round(glac_pos[1,1],2),
                         sep = " "))

source_slope_halfeye
ggsave(plot = source_slope_halfeye,
       filename = paste(write_dir, "/source_slope_halfeye.png", 
                        sep = ""))
# slope magnitude table ####
slope_mags <- data.frame(
  source = c("glacier", "sub_ice"),
  magnitude = c(
    fixef(brm1)[2,1] * (sd_temp / sd_year), #glacier
    (fixef(brm1)[2,1]+fixef(brm1)[4,1]) * (sd_temp / sd_year) # sub_ice
  ),
  text = "Degrees C per year",
  Q2.5 = c(fixef(brm1)[2,3] * (sd_temp / sd_year),
           (fixef(brm1)[2,3]+fixef(brm1)[4,3]) * (sd_temp / sd_year)),
  Q97.5 = c(fixef(brm1)[2,4] * (sd_temp / sd_year),
            (fixef(brm1)[2,4]+fixef(brm1)[4,4]) * (sd_temp / sd_year))
)
slope_mags
write_csv(slope_mags,
          file = paste(write_dir, "/slope_magnitudes.csv", 
                       sep = ""))

# source slopes ####
# source with epred draws
source_data <- brm1$data |>
  select(source, year_s) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 8,
         day = 15,
         date = make_date(year, month, day))

# # includes random effects of site and year
# add_predicted_draws(
#   newdata = source_data,
#   brm1,
#   allow_new_levels = TRUE) |>
#   #median_qi(.epred) |>
#   mutate(.prediction = (.prediction*sd_temp)+mean_temp) |>
#   ggplot(aes(x = date, 
#              y = .prediction,
#              fill = source,
#              color = source)) +
#   stat_lineribbon(aes(y = .prediction),
#                   .width = c(0.95, 0.68),
#                   alpha = 1/4) +
#   theme_bw() +
#   labs(title = "Predicted August Temperatures by Source",
#        x = "Year",
#        y = "Temperature") +
#   facet_wrap(~source) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_color_brewer(palette = "Dark2")

# plots just the fixed effect of year and source
# add_predicted_draws(
#   newdata = source_data,
#   brm1,
#   #allow_new_levels = TRUE,
#   re_formula = NA) |> # ignores random effects
#   #median_qi(.epred) |>
#   mutate(.prediction = (.prediction*sd_temp)+mean_temp) |>
#   ggplot(aes(x = date, 
#              y = .prediction,
#              fill = source,
#              color = source)) +
#   stat_lineribbon(aes(y = .prediction),
#                   .width = c(0.95, 0.68),
#                   alpha = 1/4) +
#   theme_bw() +
#   labs(title = "Predicted August Temperatures by Source",
#        x = "Year",
#        y = "Temperature") +
#   facet_wrap(~source) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_color_brewer(palette = "Dark2")

source_slope_epred <- add_epred_draws(
  newdata = source_data,
  brm1,
  #allow_new_levels = TRUE,
  re_formula = NA) |> # ignores random effects
  #median_qi(.epred) |>
  mutate(.epred = (.epred*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, 
             y = .epred,
             fill = source,
             color = source)) +
  stat_lineribbon(aes(y = .epred),
                  .width = c(0.95, 0.68),
                  alpha = 1/4) +
  theme_bw() +
  labs(title = "Change in August Temperatures by Source",
       subtitle = "Ribbons = 68 and 95% CrI",
       x = "Year",
       y = "Temperature") +
  facet_wrap(~source) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")
source_slope_epred
ggsave(plot = source_slope,
       filename = paste(write_dir, "/source_slope_epred.png", 
                        sep = ""))
# spaghetti of predictions
# fixed effects
source_spag <- add_epred_draws(newdata = source_data,
                brm1,
                re_formula = NA,
                allow_new_levels = TRUE,
                ndraws = 100) |>
  mutate(.epred = (.epred*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, y = .epred, color = source)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .5) +
  #geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~source) +
             #ncol = 2) +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       subtitle = "Spaghetti Plot, 100 draws",
       x = "Year",
       y = "Fitted Temperature")
ggsave(plot = source_spag,
       filename = paste(write_dir, "/source_slope_spag.png", 
                        sep = ""))

# this plot looks at fixed effects of year*source
c_eff <- conditional_effects(brm1)
# plot source
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

source_slopes_plot
ggsave(plot = source_slopes_plot,
       filename = paste(write_dir, "/source_slopes.png", 
                        sep = ""))



# # Add 2 new years ####
# fit_data <- brm1$data |>
#   select(source, site) |>
#   distinct()
# # add new years here
# year_s_vector <- brm1$data$year_s %>% unique() %>% sort()
# # difference between standardized years
# # max year - penultimate max year
# year_diff <- year_s_vector[length(year_s_vector)] - year_s_vector[(length(year_s_vector)-1)] 
# year_diff
# new_year <- year_s_vector[length(year_s_vector)] + year_diff
# new_year
# 
# new_years <- data.frame(year_s = c(year_s_vector, new_year,
#                                    new_year +year_diff)) # new years 
# 
# fit_data <- expand_grid(fit_data, new_years)