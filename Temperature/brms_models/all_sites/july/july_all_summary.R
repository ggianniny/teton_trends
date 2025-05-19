# Random intercept and slope for june in GTNP 

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

write_dir <- "Temperature/brms_models/all_sites/july"

brm1 <- readRDS("Temperature/brms_models/all_sites/july/fit_rand_slopes_hurdle_source.rds")
# with hu ~ source*site
# brm1 <- readRDS("Temperature/brms_models/all_sites/july/fit_rand_slopes_hurdle_sourceXsite.rds")

plot(brm1)
pairs(brm1)

brm1$formula

brm1$data |>
  distinct(site, source) |>
  arrange(source)

brm1$data |> filter(temp_1 == 0) |> count()

# posterior predictions ####
pp_check(brm1,
         type = "stat_grouped",
         group = "source")
# pp_check(brm1,
#          type = "violin_grouped",
#          group = "source")
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
         type = "dens_overlay_grouped",
         group = "w_1")
pp_check(brm1,
         type = "dens_overlay_grouped",
         group = "year_s")
pp_check(brm1,
         type = "dens_overlay_grouped",
         group = "site")
pp_check(brm1, type = "hist")
pp_check(brm1, type = "stat_2d")
#pp_check(brm1, type = "ribbon")
pp_check(brm1,
         ndraws = 100,
         type = "scatter_avg_grouped",
         group = "source")

# model summaries ####
summary(brm1)
# coef(brm1)$site[,,"year_s"]
# coef(brm1)

mod_r2 <- bayes_R2(object = brm1)
mod_r2
write_csv(as.data.frame(mod_r2), paste(write_dir, 
                        "/best_mod_r2.csv",
                        sep = ""))

conditional_effects(brm1)
conditional_effects(brm1, dpar = "hu")

# plot individual sites through time  
get_variables(brm1)

# calculate mean and sd of original data 
fit_data_orig <- readRDS(paste(write_dir, 
                               "/hurdle_fit_data.rds",
                               sep = ""))
fit_data_orig |>
  select(site, source) |>
  distinct() |>
  arrange(source)

fit_data_orig |>
  group_by(site, year) |>
  count()

fit_data_orig |> 
  mutate(day = 15, 
         date = make_date(year, month, day)) |>
  ggplot(aes(x = date, 
             y = temp_c,
             color = source)) +
  geom_point() +
  facet_wrap(source~site, scales = "free_y") +
  labs(title = "July Temperatures")



# epred draws ####
# simulate types of data you might collect
mean_year <- mean((fit_data_orig$year))
sd_year <- sd((fit_data_orig$year))
max_temp <- max((fit_data_orig$temp_c))
max_w <- max((fit_data_orig$w))


# simulate types of data you might collect
fit_data_obs_years <- brm1$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 7,
         day = 15,
         date = make_date(year, month, day))

# plot of predicting new data ####
pred_plot <- add_predicted_draws(
  newdata = fit_data_obs_years,
  brm1) |>
  median_qi(.prediction) |>
  mutate(.prediction = .prediction*max_temp,
         .lower = .lower*max_temp,
         .upper = .upper*max_temp) |>
  ggplot(aes(x = date, 
             y = .prediction,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  # geom_point(data = brm1$data |>
  #              mutate(year = (year_s*sd_year)+mean_year,
  #                     month = 6,
  #                     day = ((w_1 *max_w)-25)*5,
  #                     date = make_date(year, month, day),
  #                     temp = (temp_1*max_temp)),
  #            aes(x = date,
  #                y = temp,
  #                color = source),
  #            inherit.aes = FALSE,
  #            alpha = 0.2, 
  #            position = position_jitter(
  #              width = 35,
  #              height = NULL)) +
  facet_wrap(source~site,
             ncol = 3,
             scales = "free_y") +
  theme_bw() +
  labs(title = "Predicted Temperatures",
       subtitle = "Ribbon shows predictions for new data",
       x = "Year",
       y = "Predicted Temperature") +
  scale_fill_manual(values = c("deepskyblue", 
                               "slategrey",
                               "springgreen4")) +
  scale_color_manual(values = c("deepskyblue", 
                                "slategrey",
                                "springgreen4")) 
pred_plot

my_ggsave(plot = pred_plot,
       filename = paste(write_dir, "/Predicted_temps_july.png", 
                        sep = ""))


# plot of fitted means ####
# this is what we found
fit_plot <- add_epred_draws(
  newdata = fit_data_obs_years,
  brm1,
  allow_new_levels = TRUE) |>
  median_qi(.epred) |>
  mutate(.epred = (.epred*max_temp),
         .lower = (.lower*max_temp),
         .upper = (.upper*max_temp)) |>
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
  labs(title = "Fitted June Temperatures",
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
       filename = paste(write_dir, "/fitted_temps_june.png", 
                        sep = ""))


# source slopes ####
# source with epred draws
source_data <- brm1$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 7,
         day = 15,
         date = make_date(year, month, day))

source_slope_epred <- add_epred_draws(
  newdata = source_data,
  brm1,
  #allow_new_levels = TRUE,
  re_formula = NA) |> # ignores random effects
  #median_qi(.epred) |>
  mutate(.epred = (.epred*max_temp)) |>
  ggplot(aes(x = date, 
             y = .epred,
             fill = source,
             color = source)) +
  stat_lineribbon(aes(y = .epred),
                  .width = c(0.95, 0.68),
                  alpha = 1/4) +
  theme_bw() +
  labs(title = "Change in June Temperatures by Source",
       subtitle = "Ribbons = 68 and 95% CrI",
       x = "Year",
       y = "Temperature") +
  facet_wrap(site~source, scales = "free_x") +
  scale_fill_manual(values = c("deepskyblue",
                               "slategrey",
                               "springgreen4")) +
  scale_color_manual(values = c("deepskyblue",
                                "slategrey", 
                                "springgreen4"))
  
source_slope_epred
my_ggsave(plot = source_slope_epred,
       filename = paste(write_dir, "/source_slope_epred_june.png", 
                        sep = ""))


data.frame(year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(brm1$data$source),
              site = unique(brm1$data$site)) |>
  add_epred_draws(brm1,
                  re_formula = NA) |>
  ungroup()|>
  mutate(.epred = .epred * max_temp) |>
  select(-.row, -.chain, -.iteration, -year_s) |>
  pivot_wider(names_from = "year_real",
              values_from = ".epred") |>
  mutate(slope_new = `2020` - `2019`) |>
  # slope_new = chnage in actual temperature right scale
  mutate(percent_change = slope_new / `2019` *100) |>
  group_by(source) |>
  median_qi(percent_change)


# prob pos
glac_pos <- brm1 %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = (b_year_s) > 0) %>%
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


# half eye
data.frame(year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(temp_clean$source)) |>
  add_epred_draws(brm1,
                  re_formula = NA,
                  allow_new_levels = TRUE) |>
  ungroup()|>
  mutate(.epred = .epred * max_temp) |>
  select(-.row, -.chain, -.iteration, -year_s) |>
  pivot_wider(names_from = "year_real",
              values_from = ".epred") |>
  mutate(slope_new = `2020` - `2019`) |>
  # slope_new = chnage in actual temperature right scale
  mutate(prop = slope_new / `2019` *100) |>
  ggplot(
    aes(y = source,
        x = slope_new,
        fill = after_stat(x >0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "orangered1")) +
  theme_bw() +
  labs(x = "percentage change each year") +
  annotate("text", # rock_glacier
           x = 1,
           y = 2.2,
           label = paste("P(Positive)",
                         round(sub_pos[1,1],2),
                         sep = " ")) +
  annotate("text", # Glacier
           x = 1,
           y = 1.2,
           label = paste("P(Positive)",
                         round(glac_pos[1,1],2),
                         sep = " ")) +
  annotate("text", # snow
           x = 1,
           y = 3.3,
           label = paste("P(Positive)",
                         round(snow_pos[1,1],3),
                         sep = " ")) +
NULL
