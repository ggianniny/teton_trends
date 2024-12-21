# exploring temperature data distributions
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

temp_aug %>%
  ggplot(aes(x = temp_s)) +
  stat_halfeye() 
temp_aug %>%
  ggplot(aes(x = temp_s,
             fill = source)) +
  stat_halfeye(alpha = 0.5) 
temp_aug %>%
  ggplot(aes(x = temp_s,
             fill = as.factor(year),
             y = as.factor(year))) +
  stat_halfeye(alpha = 0.5) 

temp_aug %>%
  ggplot(aes(x = temp_c,
             fill = as.factor(year),
             y = as.factor(year))) +
  stat_halfeye(alpha = 0.5) +
  facet_wrap(~source,
             scales = "free") 


temp_aug %>%
  ggplot(aes(x = temp_c,
             color = site,
             fill = site,
             y = as.factor(year))) +
  stat_pointinterval(alpha = 0.5,
                     position = position_jitter(height = 0.25)) +
  facet_wrap(~source,
             scales = "free") 

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
