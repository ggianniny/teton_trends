# Random intercept and slope for August in GTNP 

# make sure sources match new coding:
# "sub_ice" ~ "rock_glacier",
# "snowmelt" ~ "snowfield",

library(tidyverse)
library(brms)
library(tidybayes)

my_ggsave <- function(
    filename = default_name(plot),
    height= 8,
    width= 8,
    dpi= 200, ...) {
  ggsave(filename=filename,
         height=height,
         width=width,
         dpi=dpi,
         ...)
}

write_dir <- "Temperature/brms_models/all_sites/august"

brm1 <- readRDS("Temperature/brms_models/all_sites/august/fit_rand_slopes_aug.rds")
# plot(brm1)
# pairs(brm1)

brm1$data |>
  distinct(site, source) |>
  arrange(source)

# posterior predictions ####
# pp_check(brm1,
#          type = "stat_grouped",
#          group = "source")
# pp_check(brm1,
#          type = "violin_grouped",
#          group = "source")
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
# coef(brm1)$site[,,"year_s"]
# coef(brm1)

mod_r2 <- bayes_R2(object = brm1)
mod_r2
write_csv(as.data.frame(mod_r2), paste(write_dir, 
                        "/best_mod_r2_aug.csv",
                        sep = ""))

# conditional_effects(brm1)

# plot individual sites through time  
get_variables(brm1)

# calculate mean and sd of original data 
fit_data_orig <- readRDS(paste(write_dir, 
                               "/fit_data.rds",
                               sep = ""))
fit_data_orig |>
  select(site, source) |>
  distinct() |>
  arrange(source)

# # add new years here ####
# Need to fix this, if we decide to do it ####
# year_s_vector <- brm1$data$year_s %>% unique() %>% sort()
# # difference between standardized years
# # max year - penultimate max year
# year_diff <- year_s_vector[length(year_s_vector)] - year_s_vector[(length(year_s_vector)-1)] 
# year_diff
# new_year <- year_s_vector[length(year_s_vector)] + year_diff
# new_year
# 
# new_year_df <- brm1$data |>
#   select(source, site) |>
#   distinct() |>
#   mutate(year_s = c(new_year,
#                     new_year +year_diff)) # new years 
# 
# fit_data <- bind_rows(fit_data, new_year_df)

# epred draws ####
# simulate types of data you might collect
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

# plot of predicting new data ####
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
             inherit.aes = FALSE,
             alpha = 0.2, 
             position = position_jitter(
               width = 35,
               height = NULL)) +
  facet_wrap(source~site,
             ncol = 3,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Predicted Temperatures",
       subtitle = "Ribbon shows predictions for new data",
       x = "Year",
       y = "Predicted Temperature") +
  scale_fill_manual(values = c("deepskyblue", 
                               "springgreen4",
                               "slategrey")) +
  scale_color_manual(values = c("deepskyblue", 
                                "springgreen4",
                                "slategrey")) 
pred_plot

my_ggsave(plot = pred_plot,
       filename = paste(write_dir, "/Predicted_temps_aug.png", 
                        sep = ""))


# plot of fitted means ####
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
  # geom_point(data = brm1$data |>
  #              mutate(year = (year_s*sd_year)+mean_year,
  #                     month = 8,
  #                     day = 15,
  #                     date = make_date(year, month, day),
  #                     temp = (temp_s*sd_temp)+mean_temp),
  #            aes(x = date,
  #                y = temp,
  #                color = source),
  #            inherit.aes = FALSE) +
  facet_wrap(source~site,
             scales = "free_y",
             ncol = 3) +
  theme_bw() +
  labs(title = "Fitted August Temperatures",
       subtitle = "Ribbon is 95% CrI for mean temperature",
       x = "Year",
       y = "Fitted Temperature")  +
  scale_fill_manual(values = c("deepskyblue", 
                               "springgreen4",
                               "slategrey")) +
  scale_color_manual(values = c("deepskyblue", 
                                "springgreen4",
                                "slategrey"))
fit_plot

my_ggsave(plot = fit_plot,
       filename = paste(write_dir, "/fitted_temps_aug.png", 
                        sep = ""))

# spaghetti of fits
fit_spag <- add_epred_draws(newdata = fit_data_obs_years,
                            brm1,
                            allow_new_levels = TRUE,
                            ndraws = 100) |>
  #mutate(.epred = (.epred*sd_temp)+mean_temp) |>
  ggplot(aes(x = year_s, y = .epred, color = source)) +
  geom_line(aes(y = .epred, group = paste(site, .draw)),
            alpha = .2) +
  facet_wrap(source~site, ncol = 3) +
  theme_bw() +
  labs(title = "Fitted August Temperatures",
       subtitle = "Spaghetti Plot, 100 draws",
       x = "Year",
       y = "Fitted Temperature")+
  scale_fill_manual(values = c("deepskyblue", 
                               "springgreen4",
                               "slategrey")) +
  scale_color_manual(values = c("deepskyblue", 
                                "springgreen4",
                                "slategrey")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_abline()
fit_spag

my_ggsave(plot = fit_spag,
       filename = paste(write_dir, "/fitted_spaghetti_aug.png", 
                        sep = ""))




# probability of positive slopes ####
get_variables(brm1)
# glacier
glac_pos <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = b_year_s > 0) %>%
  summarize(prob_pos = mean(year_pos)) |>
  mutate(source = "glacier")

# sub ice
sub_pos <- brm1 %>%
  spread_draws(#b_Intercept, 
    #b_sourcerock_glacier,
    b_year_s,
    `b_year_s:sourcerock_glacier`) %>%
  rename(year_sub = `b_year_s:sourcerock_glacier`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub > 0) %>%
  summarize(prob_pos = mean(sub_pos))|>
  mutate(source = "rock_glacier")


# snow
snow_pos <- brm1 %>%
  spread_draws(#b_Intercept, 
    #b_sourcesnowfield,
    b_year_s,
    `b_year_s:sourcesnowfield`) %>%
  rename(year_sub = `b_year_s:sourcesnowfield`) %>%
  mutate(sub = b_year_s + year_sub, 
         sub_pos = sub > 0) %>%
  summarize(prob_pos = mean(sub_pos))|>
  mutate(source = "snow_melt")


(prob_pos_df <- bind_rows(glac_pos, sub_pos, snow_pos))
write_csv(prob_pos_df,
          file = paste(write_dir, "/prob_slope_positive_aug.csv", 
                       sep = ""))


# distribution of source slopes ####
# halfeye plots of b_source
b_glacier <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(source = "glacier",
         b_year_s = b_year_s * (sd_temp / sd_year))

b_sub <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcerock_glacier`) %>%
  rename(year_sub = `b_year_s:sourcerock_glacier`) %>%
  mutate(b_year_s = b_year_s + year_sub,
         b_year_s = b_year_s * (sd_temp / sd_year)) %>%
  select(-year_sub) |>
  mutate(source = "rock_glacier")

b_snow <- brm1 %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowfield`) %>%
  rename(year_snow = `b_year_s:sourcesnowfield`) %>%
  mutate(b_year_s = b_year_s + year_snow,
         b_year_s = b_year_s * (sd_temp / sd_year)) %>%
  select(-year_snow) |>
  mutate(source = "snowfield")

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
  guides(fill=guide_legend(title="Positive Slope")) +
  annotate("text", # rock_glacier
           x = 0.5,
           y = 3.5,
           label = paste("P(Positive)",
                         round(sub_pos[1,1],2),
                         sep = " ")) +
  annotate("text", # Glacier
           x = 0.5, 
           y = 1.5,
           label = paste("P(Positive)",
                         round(glac_pos[1,1],2),
                         sep = " ")) +
  annotate("text", # snow
           x = 0.65, 
           y = 2.5,
           label = paste("P(Positive)",
                         round(snow_pos[1,1],3),
                         sep = " "))

source_slope_halfeye
my_ggsave(plot = source_slope_halfeye,
       filename = paste(write_dir, "/source_slope_halfeye_aug.png", 
                        sep = ""))

# slope magnitude table ####
slope_mags <- data.frame(
  source = c("glacier", "rock_glacier", "snowfield"),
  magnitude = c(
    fixef(brm1)[2,1] * (sd_temp / sd_year), #glacier
    (fixef(brm1)[2,1]+fixef(brm1)[6,1]) * (sd_temp / sd_year), # rock_glacier
    (fixef(brm1)[2,1]+fixef(brm1)[5,1]) * (sd_temp / sd_year) # snow
    
  ),
  text = "Degrees C per year",
  Q2.5 = c(fixef(brm1)[2,3] * (sd_temp / sd_year),
           (fixef(brm1)[2,3]+fixef(brm1)[6,3]) * (sd_temp / sd_year),
           (fixef(brm1)[2,3]+fixef(brm1)[5,3]) * (sd_temp / sd_year)),
  Q97.5 = c(fixef(brm1)[2,4] * (sd_temp / sd_year),
            (fixef(brm1)[2,4]+fixef(brm1)[6,4]) * (sd_temp / sd_year),
            (fixef(brm1)[2,4]+fixef(brm1)[5,4]) * (sd_temp / sd_year))
)
slope_mags
write_csv(slope_mags,
          file = paste(write_dir, "/slope_magnitudes_aug.csv", 
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
  scale_fill_manual(values = c("deepskyblue", 
                               "springgreen4",
                               "slategrey")) +
  scale_color_manual(values = c("deepskyblue", 
                                "springgreen4",
                                "slategrey"))
  
source_slope_epred
my_ggsave(plot = source_slope_epred,
       filename = paste(write_dir, "/source_slope_epred_aug.png", 
                        sep = ""))


# spaghetti of predictions
# fixed effects
source_spag <- add_epred_draws(newdata = source_data,
                               brm1,
                               re_formula = NA,
                               allow_new_levels = TRUE,
                               ndraws = 200) |>
  mutate(.epred = (.epred*sd_temp)+mean_temp) |>
  ggplot(aes(x = date, y = .epred, color = source)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .25) +
  facet_wrap(~source) +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       subtitle = "Spaghetti Plot, 200 draws",
       x = "Year",
       y = "Fitted Temperature")+
  scale_fill_manual(values = c("deepskyblue", 
                               "springgreen4",
                               "slategrey")) +
  scale_color_manual(values = c("deepskyblue", 
                                "springgreen4",
                                "slategrey"))
source_spag
my_ggsave(plot = source_spag,
       filename = paste(write_dir, "/source_slope_spag_aug.png", 
                        sep = ""))
