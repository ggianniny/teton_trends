#GTNP_aug_no_snow_ fixed slopes, only year effects

# Fixed effects of year
# random intercepts for year and site

# terminal lines to run
# r
# source("Temperature/brms_models/GTNP_sites/no_snow/august/GTNP_aug_no-snow_year_only_slopes_brms.R")

library(tidyverse)
library(brms)
library(tidybayes)

write_dir <- "Temperature/brms_models/GTNP_sites/no_snow/august"

temp_aug <- readRDS(paste(write_dir, 
                          "/fit_data.rds",
                          sep = ""))

my_prior <- c(prior(normal(0,0.5), class = b),
              prior(normal(0,0.5), class = Intercept),
              prior(exponential(2), class = sd))


brm4 <- brm(temp_s ~ year_s +
              (1|site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

# update the above model with more chains, cores, and iterations
brm4 <- update(brm4,
               chains = 4,
               cores = 4,
               iter = 2000)

saveRDS(brm4, paste(write_dir,
                    "/fit_year_slopes_aug_GTNP.rds", 
                    sep = ""))