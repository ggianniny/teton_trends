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
  select(temp_c, year, month, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source)) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_s = temp_c / mean(temp_c))

hist(temp$temp_s)

temp |>
  ggplot(aes(x = temp_s, 
             fill = source)) +
  geom_histogram(alpha = 0.5,
                 position = "identity") +
  facet_wrap(~month,
             scales = "free")

# make smaller data to figure out Hurdle model

test_dat <- temp |>
  filter(site == "skillet" |
           site == "grizzly",
         year == 2019 |
           year == 2020,
         month == 5)
#test_dat
test_dat |> distinct(site, source, year)

get_prior(temp_s ~ year_s * source +
            (1 + year_s |site) + (1|year_s),
          family = hurdle_gamma(),
          data = temp_aug)

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(1,0.5), class = Intercept),
              prior(exponential(2), class = sd))

sample_prior <- brm(temp_s ~ year_s + source +
                      year_s:source +
                      (1 + year_s |site) + (1|year_s),
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


hurdle1 <- brm(temp_s ~ year_s + source +
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


aug_dat <- temp |>
  filter(month == 8)

hurdle_aug <- brm(temp_s ~ year_s + source +
                    year_s:source +
                    (1 + year_s |site) + (1|year_s),
                  family = hurdle_gamma(),
                  data = aug_dat,
                  prior = my_prior,
                  iter = 10,
                  chains = 1)

hurdle_aug <- update(hurdle_aug,
                     iter = 600, 
                     chains = 4, 
                     cores = 4)
# save the output
saveRDS(hurdle_aug, paste(write_dir,
                    "/fit_rand_slopes_hurdle_aug.rds", 
                    sep = ""))