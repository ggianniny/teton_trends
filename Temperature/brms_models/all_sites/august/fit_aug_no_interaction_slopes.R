# 
library(tidyverse)
library(brms)
library(tidybayes)

# source("Temperature/brms_models/all_sites/august/fit_aug_no_interaction_slopes.R")

write_dir <- "Temperature/brms_models/all_sites/august"
temp_aug <- readRDS(paste(write_dir, 
                          "/fit_data.rds",
                          sep = ""))

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))
brm3 <- brm(temp_s ~ year_s + source + 
              (1 |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

brm3 <- update(
  brm3,
  chains = 4,
  cores = 4,
  iter = 2000,
  save_pars = save_pars(all = TRUE))

saveRDS(brm3, paste(write_dir,
                    "/fit_no_interaction_slopes_aug.rds", 
                    sep = ""))