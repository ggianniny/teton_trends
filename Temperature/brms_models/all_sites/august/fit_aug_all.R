# Random intercept and slope for August in GTNP 

library(tidyverse)
library(brms)
library(tidybayes)

# source("Temperature/brms_models/all_sites/august/fit_aug_all.R")

write_dir <- "Temperature/brms_models/all_sites/august"

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
         month == 8,
         !site %in% rm_sites)
unique(temp_clean$site)

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

temp_aug |>
  select(site) |> 
  distinct()

temp_aug |>
  group_by(year, site) |> 
  count() |>
  arrange(desc(year), desc(site))

# # remove windcave in 2024
# # Logger was exposed to air when data was downloaded
# 
# dim(temp_aug)
# temp_aug <- temp_aug |>
#   mutate(rm = site == "windcave" & year == 2024) |>
#   filter(rm == FALSE) |>
#   select(-rm)
# 
# temp_aug |>
#   dim()
# temp_aug |>
#   group_by(year, site) |> 
#   count() |>
#   arrange(desc(site))

saveRDS(temp_aug, paste(write_dir, 
                        "/fit_data.rds",
                        sep = ""))

# temp_aug %>%
#   ggplot(aes(x = year,
#              y = temp_c, 
#              color = site)) +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   stat_smooth(method = "lm", 
#               inherit.aes = FALSE,
#               color = "black",
#               aes(x = year, y = temp_c)) +
#   facet_wrap(~source, scales = "free_y")

get_prior(temp_s ~ year_s * source + (1 + year_s |site) + (1|year_s),
          data = temp_aug)


my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))

# fit "quick" model
# make sure it works, and avoid need to compile each time. 
brm1 <- brm(temp_s ~ year_s + source + year_s:source +
              (1 + year_s |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

# update the above model with more chains, cores, and iterations
tictoc::tic()
brm1 <- update(
  brm1,
  chains = 4,
  cores = 4,
  iter = 2000,
  save_pars = save_pars(all = TRUE))
tictoc::toc()
# 2025-01-09 = ~2.5 hours

# save the output
saveRDS(brm1, paste(write_dir,
                    "/fit_rand_slopes_aug.rds", 
                    sep = ""))

# # change models and compare
# tictoc::tic()
# brm2 <- brm(temp_s ~ year_s + source + year_s:source +
#               (1|site) + (1|year_s),
#             data = temp_aug,
#             prior = my_prior,
#             iter = 10,
#             chains = 1)
# tictoc::toc()
# 
# tictoc::tic()
# brm2 <- update(brm2,
#                chains = 4,
#                cores = 4,
#                iter = 2000,
#                save_pars = save_pars(all = TRUE))
# tictoc::toc()
# 
# saveRDS(brm2, paste(write_dir,
#                     "/fit_slopes_aug.rds", 
#                     sep = ""))

# brm3 <- brm(temp_s ~ year_s + source + 
#               (1|site) + (1|year_s),
#             data = temp_aug,
#             prior = my_prior,
#             iter = 10,
#             chains = 1)
# 
# tictoc::tic()
# brm3 <- update(brm3,
#                chains = 4,
#                cores = 4,
#                iter = 2000,
#                save_pars = save_pars(all = TRUE))
# tictoc::toc()
# saveRDS(brm3, paste(write_dir,
#                     "/fit_no-interaction-slopes_aug.rds", 
#                     sep = ""))
# 
# 
# brm4 <- brm(temp_s ~ year_s + 
#               (1|site) + (1|year_s),
#             data = temp_aug,
#             prior = my_prior,
#             iter = 10,
#             chains = 1)
# tictoc::tic()
# brm4 <- update(brm4,
#                chains = 4,
#                cores = 4,
#                iter = 2000,
#                save_pars = save_pars(all = TRUE))
# tictoc::toc()
# saveRDS(brm4, paste(write_dir,
#                     "/fit_no-source_aug.rds", 
#                     sep = ""))

