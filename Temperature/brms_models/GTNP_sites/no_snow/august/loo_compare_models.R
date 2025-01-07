# compare models

library(brms)

write_dir <- "Temperature/brms_models/GTNP_sites/no_snow/august"

brm1 <- readRDS(paste(write_dir,
                    "/fit_rand_slopes_aug_GTNP.rds", 
                    sep = ""))
brm2 <- readRDS(paste(write_dir,
                    "/fit_year_x_source_slopes_aug_GTNP.rds", 
                    sep = ""))
brm3 <- readRDS(paste(write_dir,
                    "/fit_year_source_slopes_aug_GTNP.rds", 
                    sep = ""))
brm4 <- readRDS(paste(write_dir,
                    "/fit_year_slopes_aug_GTNP.rds", 
                    sep = ""))

loo1 <- loo(brm1)#, moment_match = TRUE)
loo2 <- loo(brm2)#, moment_match = TRUE)
loo3 <- loo(brm3)#, moment_match = TRUE)
loo4 <- loo(brm4)#, moment_match = TRUE)
loo_compare(loo1, loo2, loo3, loo4)

# if this doesn't work, add following to model fits:
# update(... ,
  # save_pars = save_pars(all = TRUE),
  # ...