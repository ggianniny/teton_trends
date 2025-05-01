# hurdle model for August
# this script just fits the hurdle model for august. 
# need to make a separate script for summarizing output, making figures, etc. 

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

# source("Temperature/brms_models/all_sites/july/july_hurdle.R")

write_dir <- "Temperature/brms_models/all_sites/july"
model_month <- 7
full_iter <- 200 # 2000 for full run

# start: 2025-04-29-12:50
# end: 


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
         year = year(date1),
         w = week(date1)) |>
  filter(!is.na(temp_c),
         !site %in% rm_sites)

temp_clean <- left_join(temp_clean,
                        source_info)


temp_clean <- temp_clean |>
  filter(month == model_month)

# temp_clean |> 
#   mutate(w = w / max(w)) |>
#   distinct(w)
# # plots of data
# temp_clean |>
#   ggplot(aes(x = date1, 
#              y = temp_c)) +
#   geom_point() +
#   facet_wrap(source~site)
# 
# temp_clean |>
#   ggplot(aes(x = w / max(w), 
#              y = temp_c)) +
#   geom_point() +
#   facet_wrap(year~., scales = "free_x")
# 
# # plots of data
# temp_clean |>
#   filter(site == "s_teton") |>
#   ggplot(aes(x = date_tm, 
#              y = temp_c)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(year~., 
#              scales = "free_x")


mod_dat <- temp_clean %>% 
  select(temp_c, temp_raw, year, month, w, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source)) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_1 = temp_c / max(temp_c),
         w_1 = w / max(w))

get_prior(temp_1 ~ year_s + source +
            year_s:source + w + w:year_s +
            (1 + year_s |site) + (1|year_s),
          data = mod_dat)

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))
# slope b normal(0,1)
# intercept, gamma auto loglink function, n(0,0.5)
# hu prior leave it alone




saveRDS(mod_dat, paste(write_dir, 
                        "/hurdle_fit_data.rds",
                        sep = ""))

hurdle_mod <- brm(temp_1 ~ year_s + source +
                    year_s:source + w_1 + w_1:year_s +
                    (1 + year_s |site) + (1|year_s),
                  family = hurdle_gamma(),
                  data = mod_dat,
                  prior = my_prior,
                  iter = 10,
                  chains = 1)

hurdle_mod <- update(hurdle_mod,
                     iter = full_iter, 
                     chains = 4, 
                     cores = 4,
                     save_all_pars = TRUE)
# save the output
saveRDS(hurdle_mod, paste(write_dir,
                    "/fit_rand_slopes_hurdle.rds", 
                    sep = ""))

