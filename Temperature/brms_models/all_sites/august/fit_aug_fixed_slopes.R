# 
library(tidyverse)
library(brms)
library(tidybayes)

# source("Temperature/brms_models/all_sites/august/fit_aug_fixed_slopes.R")

write_dir <- "Temperature/brms_models/all_sites/august"
temp_aug <- readRDS(paste(write_dir, 
                        "/fit_data.rds",
                        sep = ""))

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))
brm2 <- brm(temp_s ~ year_s + source + year_s:source +
              (1 |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

brm2 <- update(
  brm2,
  chains = 4,
  cores = 4,
  iter = 2000,
  save_pars = save_pars(all = TRUE))

saveRDS(brm2, paste(write_dir,
                    "/fit_fixed_slopes_aug.rds", 
                    sep = ""))