#GTNP_aug_no_snow_ fixed slopes

# Fixed effects of year*source
# random intercepts for year and site

# terminal lines to run
# r
# source("Temperature/brms_models/GTNP_sites/no_snow/august/GTNP_aug_no-snow_fixed_slopes_brms.R")

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

# fit "quick" model
# make sure it works, and avoid need to compile each time. 
brm2 <- brm(temp_s ~ year_s * source +
              (1|site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

# update the above model with more chains, cores, and iterations
brm2 <- update(brm2,
               chains = 4,
               cores = 4,
               iter = 2000)

saveRDS(brm2, paste(write_dir,
                    "/fit_year_x_source_slopes_aug_GTNP.rds", 
                    sep = ""))

