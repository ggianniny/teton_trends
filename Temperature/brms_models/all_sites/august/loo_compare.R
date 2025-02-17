# compare models

library(brms)
write_dir <- "Temperature/brms_models/all_sites/august"

brm1 <- readRDS(paste(write_dir,
                      "/fit_rand_slopes_aug.rds", 
                      sep = ""))
brm2 <- readRDS(paste(write_dir,
                      "/fit_fixed_slopes_aug.rds", 
                      sep = ""))
brm3 <- readRDS(paste(write_dir,
                      "/fit_no_interaction_slopes_aug.rds", 
                      sep = ""))
brm4 <- readRDS(paste(write_dir,
                      "/fit_year_only_slopes_aug.rds", 
                      sep = ""))

loo1 <- loo(brm1) #, moment_match = TRUE)
loo2 <- loo(brm2) #, moment_match = TRUE)
loo3 <- loo(brm3) #, moment_match = TRUE)
loo4 <- loo(brm4) #, moment_match = TRUE)
loo_compare(loo1, loo2, loo3, loo4)

# if doesn't work, uncomment moment_match