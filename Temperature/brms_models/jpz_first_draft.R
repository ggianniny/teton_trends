library(tidyverse)
library(brms)
library(tidybayes)


source_info <- read.csv("source_info.csv")%>%
  rename(site = stream) #rename for merge

#Temperature data:

temp_clean <- read_csv("Temperature/cleaned_full_datasets/temps_hourly.csv")

temp_clean <- left_join(temp_clean,
                        source_info)

temp_clean %>% 
  filter(is.na(source))

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

temp_aug %>%
  distinct(source, site) %>%
  arrange(source)

temp_aug %>%
  distinct(source, site, year) %>%
  arrange(source)

temp_aug %>%
  ggplot(aes(x = year_s,
             y = temp_s, 
             color = site)) +
  geom_point() +
  facet_wrap(~source, scales = "free_y")

temp_aug %>%
  ggplot(aes(x = year_s,
             y = temp_s, 
             color = site)) +
  stat_pointinterval(
    position = position_dodge(width = 0.25)
  ) +
  facet_wrap(~source, scales = "free_y")


# brms model --------------------------------------------------------------


get_prior(temp_s ~ year_s * source + (1 |site) + (1|year_s),
          data = temp_aug)


my_prior <- c(prior(normal(0,0.5), class = b),
              prior(exponential(2), class = sd))


brm1 <- brm(temp_s ~ year_s * source +
              (1 |site) + (1|year_s),
            data = temp_aug,
            prior = my_prior,
            iter = 10,
            chains = 1)

brm1 <- update(brm1,
               chains = 4,
               cores = 4,
               iter = 2000)

# brm1 %>% 
#   prior_summary() %>% 
#   parse_dist(prior) %>% 
#   ggplot(aes(y = class, dist = .dist, args = .args)) + 
#   stat_dist_halfeyeh()

plot(brm1)

pp_check(brm1)
pp_check(brm1, type = "AAA")
pp_check(brm1, type = "dens_overlay_grouped", group = "source")
# pp_check(brm1, type = "violin_grouped", group = "source")
pp_check(brm1, type = "boxplot")
pp_check(brm1, type = "stat")

pp_data <- pp_check(brm1,
         type = "stat_grouped",
         group = "source")
pp_data$data # --> mutate(diff = ...) find raw data???
# maybe built in bayes_p function??

bayes_R2(object = brm1) # proportion of variance explained (ish)
# explains ~55% +- 0.2%

conditional_effects(brm1, spaghetti = TRUE, ndraws = 100)
plot(conditional_effects(brm1), points = TRUE)

conditional_effects(brm1)


saveRDS(brm1, 
        "Temperature/brms_models/fit_temp_yearXsource_rand_site_year.rds")

# brm1 <- readRDS("Temperature/brms_models/fit_temp_yearXsource_rand_site_year.rds")

# sample posts ####
post_preds <- brm1$data %>%
  select(-temp_s) %>%
  distinct() %>%
  add_epred_draws(brm1,
                  re_formula = NULL,
                  ndraws = 500) %>%
  ungroup()

post_preds %>%
  ggplot(aes(x = year_s,
             y = .epred,
             color = site)) +
  stat_pointinterval(alpha = 0.25, 
                     size = 1,
                     position = position_dodge(width = 0.15)) +
  facet_wrap(~source)

saveRDS(post_preds, 
        "Temperature/brms_models/post_preds.rds")
# post_preds <- readRDS("Temperature/brms_models/post_preds.rds")

sample_t_bar <- temp_aug %>%
  group_by(year_s) %>%
  reframe(t_bar = mean(temp_s),
          t_median = median(temp_s))

# Posterior Prediction Plots ----------------------------------------------

# compare the overall yearly-mean with overall yearly-posterior
# add source as a group

# post_t_bar <- post_preds %>%
#   group_by(year_s, source, site, .draw) %>%
#   reframe(t_bar = mean(.epred),
#           t_median = median(.epred))

post_preds %>%
  ggplot(aes(x = year_s,
             y = .epred,
             group = site)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2,
                     aes(color = "posterior",
                         shape = "posterior")) +
  geom_point(data = sample_t_bar,
             aes(y = t_bar,
                 color = "raw data",
                 shape = "raw data"),
             size = 1) +
  scale_color_manual(values = c("#56b4e9", "black")) +
  guides(shape = "none") +
  facet_wrap(~source) +
  labs(x = "Year, Z-score",
       y = "Mean temperature, Z-score") +
  NULL
  
  
post_preds %>%
  ggplot(aes(x = year_s,
             y = .epred)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2, 
                     aes(color = "posterior",
                         shape = "posterior"),) +
  geom_point(data = sample_t_bar,
             aes(y = t_median,
                 color = "raw data",
                 shape = "raw data"),
             size = 1) +
  scale_color_manual(values = c("#56b4e9", "black")) +
  guides(shape = "none") +
  #facet_wrap(~source, scales = "free") +
  labs(x = "Year, Z-score",
       y = "Median temperature, Z-score") +
  NULL


# Bayes P -----------------------------------------------------------------

(bayes_p = post_preds %>% 
  group_by(year_s, source, site, .draw) %>% 
  reframe(t_bar_post = mean(.epred)) %>% 
  left_join(sample_t_bar) %>% 
  mutate(diff = t_bar_post - t_bar,
         maxdraws = max(.draw)) %>% 
  group_by(year_s, source, site, maxdraws) %>% 
  reframe(sumdiff = sum(diff >0)) %>% 
  mutate(bayes_p = sumdiff/maxdraws))

range(bayes_p$bayes_p)
mean(bayes_p$bayes_p)
sd(bayes_p$bayes_p)
bayes_p %>% 
  filter(bayes_p > 0.9 | bayes_p < 0.1)


# remove random year ------------------------------------------------------

brm2 <- update(brm1,
               formula. = ~ . - (1|year_s),
               cores = 4, 
               chains = 4)
