# hurdle model for August
# this script just fits the hurdle model for august. 
# need to make a separate script for summarizing output, making figures, etc. 

tictoc::tic()

library(tidyverse)
library(brms)
library(tidybayes)

#===================================#
#===================================#
#===================================#
### Make sure these are updated ####
#===================================#
#===================================#
#===================================#

# source("Temperature/brms_models/all_sites/june/june_hurdle.R")

write_dir <- "Temperature/brms_models/all_sites/june"
model_month <- 6
full_iter <- 500
chains <- 6

# start 2025-03-25-11:15
# end: 
# 2025-03-25 run took ~ 22 hours

### Make sure above is updated ####
#===================================#
#===================================#
#===================================#

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

# sites to remove
rm_sites <- c("silver_run", "death_canyon", "peterson", "quad_cr", "schoolroom", "paintbrush", "windcave")

temp_clean <- temp_clean |>
  mutate(month = month(date1),
         year = year(date1)) |>
  filter(!is.na(temp_c),
         !site %in% rm_sites)

temp_clean <- left_join(temp_clean,
                        source_info)

# temp_clean |>
#   filter(month == 6,
#          site == "s_teton") |>
#   ggplot(aes(x = date_tm,
#              y = temp_c)) +
#   geom_point() +
#   facet_wrap(~year, scales = "free_x")

temp_clean |>
  filter(month == 6,
         source == "glacier") |>
  ggplot(aes(x = date_tm,
             y = temp_c,
             color = site)) +
  geom_point() +
  facet_wrap(~year, scales = "free_x")

temp_clean |>
  filter(month == 6,
         source == "glacier") |>
  ggplot(aes(x = date1,
             y = temp_c,
             color = site)) +
  geom_point() +
  facet_wrap(~year, scales = "free_x")

temp_clean |>
  filter(month == 6,
         source == "glacier") |>
  mutate(w = week(date1)) |>
  ggplot(aes(x = temp_c,
             fill = as.factor(w))) +
  geom_density() +
  facet_wrap(site~year, scales = "free_y")

temp_clean |>
  filter(month == 6,
         source == "snowfield") |>
  mutate(w = week(date1)) |>
  ggplot(aes(x = temp_c,
             fill = as.factor(w))) +
  geom_density() +
  facet_wrap(site~year, scales = "free_y")



# temp_clean |>
#   filter(month == 6) |>
#   ggplot(aes(x = date_tm,
#              y = temp_c,
#              color = site)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(source~year, scales = "free_x") +
#   theme_bw()
# temp_clean |>
#   filter(month == 6) |>
#   ggplot(aes(#x = date_tm,
#              x = temp_c,
#              fill = site)) +
#   #geom_point(alpha = 0.5) +
#   geom_density(alpha = 0.5)+
#   facet_wrap(~year, scales = "free")
# temp_clean |>
#   filter(month == 6) |>
#   ggplot(aes(#x = date_tm,
#     x = temp_c,
#     fill = as.factor(year))) +
#   #geom_point(alpha = 0.5) +
#   geom_density(alpha = 0.5)+
#   facet_wrap(~site, scales = "free")
# 
# temp_clean |>
#   filter(month == 6) |>
#   ggplot(aes(#x = date_tm,
#     x = temp_c,
#     fill = after_stat(x ==0))) +
#   #geom_point(alpha = 0.5) +
#   #geom_density(alpha = 0.5)+
#   geom_histogram() 
# 
# temp_clean |>
#   filter(month == 6) |>
#   mutate(is_0 = temp_c == 0) |>
#   summarize(sum(is_0) / n())
# 
# # daily means
# temp_clean |>
#   group_by(source, site, date1, year) |>
#   filter(month == 6) |>
#   summarize(d_mean = mean(temp_c)) |>
#   ggplot(aes(x = date1,
#              y = d_mean,
#              color = site)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(source~year, scales = "free_x") +
#   theme_bw() +
#   stat_smooth(method = "lm")
# temp_clean |>
#   group_by(source, site, date1, year) |>
#   filter(month == 8) |>
#   summarize(d_mean = mean(temp_c)) |>
#   ggplot(aes(x = date1,
#              y = d_mean,
#              color = site)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(source~year, scales = "free_x") +
#   theme_bw() +
#   stat_smooth(method = "lm")


temp <- temp_clean %>% 
  filter(!is.na(temp_c),
         !is.na(source)) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         mday = mday(date1),
         day01 = mday / max(mday)) |>
  select(temp_c, temp_raw, year, month, day01, source, site, year_s)

mod_dat <- temp |>
  filter(month == model_month) |>
  mutate(temp_1 = temp_c / max(temp_c))
saveRDS(mod_dat, paste(write_dir,
                       "/hurdle_fit_data_iter_",
                       full_iter,
                       "_",
                       Sys.Date(),
                       ".rds", 
                       sep = ""))

  
model <- bf(temp_1 ~ year_s + source +
              year_s:source + site + day01 + site:day01 + # just site offsets?
              (1 + year_s |site) + (1|year_s),
            hu ~ 1)

get_prior(model,
                  family = hurdle_gamma(),
                  data = mod_dat)

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd),
              #prior(normal(0,0.5), dpar = "hu", class = b),
              prior(normal(0,0.5), dpar = "hu", class = Intercept))
# slope b normal(0,1)
# intercept, gamma auto loglink function, n(0,0.5)
# hu prior leave it alone
# hu is probability of being a zero





# hurdle_mod <- brm(temp_1 ~ year_s + source +
#                     year_s:source +
#                     (1 + year_s |site) + (1|year_s),
#                   family = hurdle_gamma(),
#                   data = mod_dat,
#                   prior = my_prior,
#                   iter = 10,
#                   chains = 1)

# conditional_effects(hurdle_mod, dpar = "hu")
hurdle_mod <- brm(model,
                  family = hurdle_gamma(),
                  data = mod_dat,
                  prior = my_prior,
                  iter = 10,
                  chains = 1)
# Intercept
# exp(intercept) = mean response
# hu intercept
#plogis(hu_intercept) = probability of being 0


# conditional_effects(hurdle_mod2, dpar = "hu")



hurdle_mod <- update(hurdle_mod,
                     iter = full_iter, 
                     chains = chains, 
                     cores = chains,
                     save_pars = save_pars(all = TRUE))
# save the output
saveRDS(hurdle_mod, paste(write_dir,
                    "/fit_rand_slopes_hurdle_iter_",
                    full_iter,
                    "_",
                    Sys.Date(),
                    ".rds", 
                    sep = ""))

run <- tictoc::toc()
saveRDS(run,
        paste(write_dir,
              "/run_time_hurdle_iter_",
              full_iter,
              "_",
              Sys.Date(),
              ".rds", 
              sep = ""))