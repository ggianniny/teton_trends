# Random intercept and slope for August in GTNP 

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/GTNP_sites/all_sites/august"

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

saveRDS(temp_aug, paste(write_dir, 
                        "/fit_data.rds",
                        sep = ""))

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
# 2024-12-03 = (all 2024 data) = 

# save the output
saveRDS(brm1, paste(write_dir,
                    "/fit_rand_slopes_aug_GTNP.rds", 
                    sep = ""))