# hurdle model for August

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/all_sites/august"


# modify data #### 

# rescale data by dividing by mean
### Makes intercept 1, values can still be 0 or positive. 
### 

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
unique(source_info$source)

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

unique(temp_clean$site)
# All sites
# which one is intermittent now?
# Skillet, Teton, Middle Teton, 
# Cloudveil (maybe not actually a glacier?)
# Mt St John (gusher), South Cascade, Alaska Basin, Wind Cave
# Grizzly, Painbrush, N Fork Teton, S Fork Teton

# sites to remove
rm_sites <- c("silver_run", "death_canyon", "peterson", "quad_cr", "schoolroom", "paintbrush", "windcave")

temp_clean <- temp_clean |>
  mutate(month = month(date1),
         year = year(date1)) |>
  filter(!is.na(temp_c),
         !site %in% rm_sites)
unique(temp_clean$site)

hist(temp_clean$temp_c)

temp_clean <- left_join(temp_clean,
                        source_info)

temp_clean |>
  ggplot(aes(x = temp_c, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

temp <- temp_clean %>% 
  select(temp_c, temp_raw, year, month, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source)) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_1 = temp_c / max(temp_c))

hist(temp$temp_1)

temp |>
  ggplot(aes(x = temp_1, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

temp |>
  ggplot(aes(x = temp_raw, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

temp |>
  filter(month == 8) |>
  ggplot(aes(x = temp_c, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

temp |>
  filter(month == 7 | month == 6) |>
  ggplot(aes(x = temp_c, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

temp |>
  filter(month == 7 | month == 6 | month == 8) |>
  ggplot(aes(x = temp_c, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  # facet_wrap(~month,
  #            scales = "free")
  NULL

# make smaller data to figure out Hurdle model

# test_dat <- temp |>
#   filter(site == "skillet" |
#            site == "grizzly",
#          year == 2019 |
#            year == 2020,
#          month == 5)
# #test_dat
# test_dat |> distinct(site, source, year)

get_prior(temp_1 ~ year_s * source +
            (1 + year_s |site) + (1|year_s),
          family = hurdle_gamma(),
          data = temp)

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))
# slope b normal(0,1)
# intercept, gamma auto loglink function, n(0,0.5)
# hu prior leave it alone

# time run --> divide by max instead of mean, data range would be 0/1

sample_prior <- brm(temp_s ~ year_s + source +
                      year_s:source +
                      (1 + year_s |site) + (1|year_s),
                    #bf(temp_s ~year_s..., hu ~ season) # IF we have a larger model with multiple/all monts in it
                    # could make it the same as the temp data
                    family = hurdle_gamma(),
                    data = test_dat,
                    prior = my_prior,
                    sample_prior = "only",
                    iter = 10,
                    chains = 1)

sample_prior <- update(sample_prior,
                       iter = 4000)

#plot(sample_prior)
pp_check(sample_prior,
         type = "dens_overlay_grouped",
         group = "source")

# # simulate types of data you might collect
mean_year <- mean((temp$year))
sd_year <- sd((temp$year))
# mean_temp <- mean((fit_data_orig$temp_c))
# sd_temp <- sd((fit_data_orig$temp_c))

# simulate types of data you might collect
fit_data_obs_years <- sample_prior$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 5,
         day = 15,
         date = make_date(year, month, day))

# plot of predicting new data ####
add_predicted_draws(
  newdata = fit_data_obs_years,
  sample_prior,
  allow_new_levels = TRUE) |>
  #median_qi(.prediction) |>
  # mutate(.prediction = (.prediction*sd_temp)+mean_temp,
  #        .lower = (.lower*sd_temp)+mean_temp,
  #        .upper = (.upper*sd_temp)+mean_temp) |>
  ggplot(aes(x = year, 
             y = .prediction,
             # ymin = .lower, 
             # ymax = .upper,
             # fill = source,
             color = source)) +
  # geom_ribbon(alpha = 0.5) +
  # geom_line() +
  geom_point() +
  facet_wrap(~site,
             scales = "free_y") +
  theme_bw() 

add_predicted_draws(
  newdata = fit_data_obs_years,
  sample_prior,
  allow_new_levels = TRUE) |>
  ggplot(aes(x = .prediction,
             fill = source)) +
  geom_histogram(binwidth = 1,
                 position = "identity")


hurdle1 <- brm(temp_1 ~ year_s + source +
                      year_s:source +
                      (1 + year_s |site) + (1|year_s),
                    family = hurdle_gamma(),
                    data = test_dat,
                    prior = my_prior,
                    iter = 10,
                    chains = 1)

hurdle1 <- update(hurdle1, 
                  iter = 1000,
                  chains = 4, 
                  cores = 4)
pp_check(hurdle1,
         type = "dens_overlay_grouped",
         group = "source")

# boxplot
pp_check(hurdle_aug,
         type = "violin_grouped",
         group = "source")

bayes_R2(object = hurdle_aug)

aug_dat <- temp |>
  filter(month == 8)

hurdle_aug <- brm(temp_1 ~ year_s + source +
                    year_s:source +
                    (1 + year_s |site) + (1|year_s),
                  family = hurdle_gamma(),
                  data = aug_dat,
                  prior = my_prior,
                  iter = 10,
                  chains = 1)

hurdle_aug <- update(hurdle_aug,
                     iter = 2000, 
                     chains = 4, 
                     cores = 4,
                     save_all_pars = TRUE)
# save the output
saveRDS(hurdle_aug, paste(write_dir,
                    "/fit_rand_slopes_hurdle_aug.rds", 
                    sep = ""))

# hurdle_aug <- readRDS(
#   paste(
#     write_dir,
#     "/fit_rand_slopes_hurdle_aug.rds",
#     sep = ""))


pp_check(hurdle_aug)+
  labs(title = "hurdle gamma")
pp_check(hurdle_aug, type = "dens_overlay_grouped",
         group = "source")+
  labs(title = "hurdle gamma")
pp_check(hurdle_aug, 
         type = "boxplot") +
  labs(title = "hurdle gamma")

hurd_r2 <- bayes_R2(object = hurdle_aug)
hurd_r2

bayes_factor(hurdle_aug, brm1)

fit_data_obs_years <- hurdle_aug$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 8,
         day = 15,
         date = make_date(year, month, day))


add_epred_draws(
  newdata = fit_data_obs_years,
  hurdle_aug,
  allow_new_levels = TRUE, 
  dpar = TRUE) |>
  median_qi(.epred) |>
  mutate(.epred = (.epred*max(temp$temp_c)),
         .lower = (.lower*max(temp$temp_c)),
         .upper = (.upper*max(temp$temp_c))) |>
  ggplot(aes(x = date, 
             y = .epred,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  facet_wrap(source~site,
             scales = "free_y",
             ncol = 3)

# .epred, model 0, everything adjusted
# dpar = TRUE
# mu == gamma predict of only positive
# hu == probability of getting a 0
# 1 - hu probability of getting 1 (is a nonzero)
# .epred = mu * (1 - hu)

# epred multiplier, prob(!0) * if !0--> gamma, 
# two posteriors, together 
# multiply estimated mean by probabilities, actual average will be lower
# might have a "gap" from 0's and non zeros


# # Gamma ####  
# 
# get_prior(temp_s ~ year_s * source +
#             (1 + year_s |site) + (1|year_s),
#           family = Gamma(),
#           data = aug_dat)
# 
# gamma_aug <- brm(temp_s ~ year_s + source +
#                     year_s:source +
#                     (1 + year_s |site) + (1|year_s),
#                   family = Gamma(),
#                   data = aug_dat,
#                   prior = my_prior,
#                   iter = 10,
#                   chains = 1)




source_data <- hurdle_aug$data |>
  select(source, year_s) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 8,
         day = 15,
         date = make_date(year, month, day))

source_slope_epred <- add_epred_draws(
  newdata = source_data,
  hurdle_aug,
  #allow_new_levels = TRUE,
  re_formula = NA) |> # ignores random effects
  #median_qi(.epred) |>
  mutate(.epred = (.epred)*max(temp$temp_c)) |>
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
                                "slategrey",
                               "springgreen4")) +
  scale_color_manual(values = c("deepskyblue", 
                                "slategrey",
                                "springgreen4"))

source_slope_epred

max_temp <- max(temp$temp_c)

# pr(positive)

# glacier
glac_pos <- hurdle_aug %>%
  spread_draws(b_year_s) %>%
  mutate(year_pos = (b_year_s) > 0) %>%
  summarize(prob_pos = mean(year_pos)) |>
  mutate(source = "glacier")

# sub ice
sub_pos <- hurdle_aug %>%
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
snow_pos <- hurdle_aug %>%
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




# halfeye
b_glacier <- hurdle_aug %>%
  spread_draws(b_year_s) %>%
  mutate(source = "glacier",
         b_year_s = (b_year_s * (max_temp / sd_year)))

# set up raw temp as avergae

data.frame(year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(temp$source)) |>
  add_epred_draws(hurdle_aug,
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
  group_by(source) |>
  mean_qi(prop)


data.frame(year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(temp$source)) |>
  add_epred_draws(hurdle_aug,
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
  labs(x = "percentage change each year")
  
# pp_checks compare different distirbution families with pp_check
# dens overaly
# boxplot
# bayes p target = 0.5

b_sub <- hurdle_aug %>%
  spread_draws(b_year_s,
               `b_year_s:sourcerock_glacier`) %>%
  rename(year_sub = `b_year_s:sourcerock_glacier`) %>%
  mutate(b_year_s = b_year_s + year_sub,
         b_year_s = (b_year_s * max_temp / sd_year)) %>%
  select(-year_sub) |>
  mutate(source = "rock_glacier")

b_snow <- hurdle_aug %>%
  spread_draws(b_year_s,
               `b_year_s:sourcesnowfield`) %>%
  rename(year_snow = `b_year_s:sourcesnowfield`) %>%
  mutate(b_year_s = b_year_s + year_snow,
         b_year_s = (b_year_s * max_temp / sd_year)) %>%
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
  # annotate("text", # rock_glacier
  #          x = 0.5,
  #          y = 3.5,
  #          label = paste("P(Positive)",
  #                        round(sub_pos[1,1],2),
  #                        sep = " ")) +
  # annotate("text", # Glacier
  #          x = 0.5, 
  #          y = 1.5,
  #          label = paste("P(Positive)",
  #                        round(glac_pos[1,1],2),
  #                        sep = " ")) +
  # annotate("text", # snow
  #          x = 0.65, 
  #          y = 2.5,
  #          label = paste("P(Positive)",
  #                        round(snow_pos[1,1],3),
  #                        sep = " "))
NULL

source_slope_halfeye



source_spag <- add_epred_draws(newdata = source_data,
                               hurdle_aug,
                               re_formula = NA,
                               allow_new_levels = TRUE,
                               ndraws = 200) |>
  mutate(.epred = (.epred)*max_temp) |>
  ggplot(aes(x = date, y = .epred, color = source)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .25) +
  facet_wrap(~source) +
  theme_bw() +
  labs(title = "Fitted August Temperatures in GTNP",
       subtitle = "Spaghetti Plot, 200 draws",
       x = "Year",
       y = "Fitted Temperature")+
  scale_fill_manual(values = c("deepskyblue", 
                               "slategrey",
                               "springgreen4")) +
  scale_color_manual(values = c("deepskyblue",
                                "slategrey",
                                "springgreen4"))
source_spag

conditional_effects(hurdle_aug, 
                    effects = "year_s:source", 
                    spaghetti = TRUE,
                    ndraws= 100) +
  facet_wrap(.~source)

c_eff <- conditional_effects(hurdle_aug, 
                             effects = "year_s:source", 
                             spaghetti = TRUE,
                             ndraws= 100)

str(c_eff)
plot(c_eff[[1]]) +facet_wrap("source")




fit_data_obs_years <- hurdle_aug$data |>
  select(source, year_s, site) |>
  distinct() |>
  mutate(year = (year_s*sd_year)+mean_year,
         month = 8,
         day = 15,
         date = make_date(year, month, day),
         source = factor(
           source,
           levels = c(
             "snowfield",
             "glacier",
             "rock_glacier")))

# plot of predicting new data ####
pred_plot <- add_predicted_draws(
  newdata = fit_data_obs_years,
  hurdle_aug,
  allow_new_levels = TRUE) |>
  median_qi(.prediction) |>
  mutate(.prediction = (.prediction*max_temp),
         .lower = (.lower*max_temp),
         .upper = (.upper*max_temp),
         source = factor(
           source,
           levels = c(
             "snowfield",
             "glacier",
             "rock_glacier"))) |>
  ggplot(aes(x = date, 
             y = .prediction,
             ymin = .lower, 
             ymax = .upper,
             fill = source,
             color = source)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(data = hurdle_aug$data |>
               mutate(year = (year_s*sd_year)+mean_year,
                      month = 8,
                      day = 15,
                      date = make_date(year, month, day),
                      temp = (temp_1*max_temp),
                      source = factor(
                        source,
                        levels = c(
                          "snowfield",
                          "glacier",
                          "rock_glacier"))),
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
  scale_fill_manual(values = c("springgreen4",
                               "deepskyblue", 
                                "slategrey")) +
  scale_color_manual(values = c("springgreen4",
                                "deepskyblue", 
                                "slategrey")) 
pred_plot
