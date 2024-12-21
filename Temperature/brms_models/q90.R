library(tidyverse)
library(brms)
library(tidybayes)


source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

temp_clean <- left_join(temp_clean,
                        source_info)
temp_aug <- temp_clean %>% 
  mutate(month = month(date1),
         year = year(date1))%>%
  select(temp_c, year, month, source, site) %>%
  filter(!is.na(temp_c),
         !is.na(source),
         !year == 2023,
         month == 8) %>%
  mutate(year_s = (year - mean(year)) / sd(year),
         temp_s = (temp_c - mean(temp_c)) / sd(temp_c))


# get_prior(bf(temp_s ~ year_s * source + (1 + year |site) + (1|year_s),
#              quantile = 0.10),
#           family = asym_laplace(),
#           data = temp_aug)


my_prior <- c(prior(normal(0,0.06), 
                    # Niedrist and Fureder found increase of 
                    # 0.6c per decade
                    # using this to justify a prior of
                    # normal(0,0.06)
                    # plot(density(rnorm(1000, 0, 0.06)))
                    # i.e., 95% of prior weight is between 
                    # - 0.12 and +0.12 degrees C per year
                    class = b),
              prior(normal(0,0.5), 
                    class = Intercept),
              prior(exponential(2), class = sd),
              prior(lkj(4), class = cor))


q90 <- brm(bf(temp_s ~ year_s * source +
              (1 + year_s |site) + (1|year_s),
              quantile = 0.9),
            family = asym_laplace(),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

start_time <- Sys.time()
q90 <- update(q90,
               chains = 4,
               cores = 4,
               iter = 2000)
end_time <- Sys.time()
run <- end_time - start_time

saveRDS(q90, "Temperature/brms_models/q90.rds")
saveRDS(run, "Temperature/brms_models/q90_run.rds")
