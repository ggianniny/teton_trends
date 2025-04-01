# august hurdle

# rescale data by dividing by the global max
# hurdle model for August

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/all_sites/august"


# modify data #### 

# rescale data by dividing by max
### scale of 0 to 1. What is the intercept?
# source("Temperature/brms_models/all_sites/august/aug_hurdle.R")

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

aug_dat <- temp |>
  filter(month == 8)


get_prior(temp_1 ~ year_s * source +
            (1 + year_s |site) + (1|year_s),
          family = hurdle_gamma(),
          data = aug_dat)

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))
# slope b normal(0,1)
# intercept, gamma auto loglink function, n(0,0.5)
# hu prior leave it alone



aug_dat <- temp |>
  filter(month == 8)
saveRDS(aug_dat, paste(write_dir, 
                        "/hurdle_fit_data.rds",
                        sep = ""))

hurdle_aug <- brm(temp_1 ~ year_s + source +
                    year_s:source +
                    (1 + year_s |site) + (1|year_s),
                  family = hurdle_gamma(),
                  data = aug_dat,
                  prior = my_prior,
                  iter = 10,
                  chains = 1)

hurdle_aug <- update(hurdle_aug,
                     iter = 10, #2000, 
                     chains = 4, 
                     cores = 4,
                     save_all_pars = TRUE)
# save the output
saveRDS(hurdle_aug, paste(write_dir,
                    "/fit_rand_slopes_hurdle_aug.rds", 
                    sep = ""))

