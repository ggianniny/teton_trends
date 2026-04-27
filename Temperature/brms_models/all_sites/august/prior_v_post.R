# priors vs posterior distributions

library(tidyverse)
library(tidybayes)
library(brms)

brm1 <- readRDS(("Temperature/brms_models/all_sites/august/fit_rand_slopes_hurdle_aug_rand_site.rds"))

# updated brm1, should now have prior samples

### hypotesis() ??? ###
# plot(hypothesis) ==> prior v post graph???

brm1$prior

get_variables(brm1)
plot(density((rnorm(1000, sd = 0.5))))

# Intercept
spread_draws(brm1, b_Intercept) |>
  mutate(
    post = (b_Intercept),
    prior = (rnorm(n(), mean = 0, sd = 0.5))) |>
  pivot_longer(cols = c(post, prior)) |>
  ggplot(aes(x = value,
             y = name, 
             fill = name)) +
  stat_dist_halfeye(
    alpha = 0.5
  ) +
  xlim(-2.5, 1)

b_source <- get_variables(brm1)[c(1, 3, 4)]

# Intercept prior and posterior
spread_draws(
  brm1, 
  b_Intercept, 
  b_sourcerock_glacier,
  b_sourcesnowfield
) |>
  mutate(
    b_glacier = exp(b_Intercept),
    b_rock_glacier = 
      exp(b_sourcerock_glacier + b_Intercept),
    b_snowfield = 
      exp(b_sourcesnowfield + b_Intercept)) |>
  pivot_longer(
    cols = b_glacier:b_snowfield, 
    names_to = "coef",
    values_to = "posterior") |>
  select(coef, posterior) |>
  group_by(coef) |>
  mutate(prior = exp(rnorm(n(), mean = 0, sd = 0.5))) |>
  pivot_longer(posterior:prior) |>
  ggplot(aes(x = value,
             y = coef, 
             fill = name)) +
  stat_halfeye(
    alpha = 0.5
  ) +
  labs(y = "Group Intercept Coefficient")

# group-specific year coefficients
spread_draws(
  brm1, 
  b_year_s, 
  `b_year_s:sourcerock_glacier`,
  `b_year_s:sourcesnowfield`
) |>
  mutate(
    b_glacier = 
      exp(b_year_s),
    b_rock_glacier = 
      exp(`b_year_s:sourcerock_glacier` + b_year_s),
    b_snowfield = 
      exp(`b_year_s:sourcesnowfield` + b_year_s)) |>
  pivot_longer(
    cols = b_glacier:b_snowfield, 
    names_to = "coef",
    values_to = "posterior") |>
  select(coef, posterior) |>
  group_by(coef) |>
  mutate(prior = (rnorm(n(), mean = 0, sd = 0.5))) |>
  pivot_longer(posterior:prior) |>
  ggplot(aes(x = value,
             y = coef, 
             fill = name)) +
  stat_halfeye(
    alpha = 0.5
  ) +
  labs(y = "Group Year Coefficient")


spread_draws(
  brm1, 
  b_year_s) |>
  mutate(prior = (rnorm(n(), mean = 0, sd = 0.5))) |>
  pivot_longer(b_year_s:prior) |>
  ggplot(aes(x = value,
             y = name, 
             fill = name)) +
  stat_halfeye(
    alpha = 0.5
  ) +
  labs(y = "Group Year Coefficient") 


draws <- as_draws(brm1)
plot(density((draws$`1`$b_year_s)))

data.frame(post_year = (draws$`1`$b_year_s),
           prior_year = (rnorm(1000, mean = 0, sd = 0.5))) |> 
  pivot_longer(post_year:prior_year) |>
  ggplot(aes(x = value, 
             fill = name)) +
  stat_halfeye()

data.frame(post_year = (draws$`1`$b_Intercept),
           prior_year = (rnorm(1000, mean = -.75, sd = 0.5))) |> 
  pivot_longer(post_year:prior_year) |>
  ggplot(aes(x = value, 
             fill = name)) +
  stat_halfeye(alpha = 0.75)

brm1$data |>
  distinct(source) |>
  expand_grid(year_s = 0) |>
  add_epred_draws(brm1, 
                  re_formula = NA, ndraws = 1000) |>
  mutate(prior = rnorm(n(), mean = 0, sd = 0.5)) |>
  pivot_longer(.epred:prior) |>
  ggplot(aes(x = value, 
             y = source, 
             fill = name)) +
  stat_halfeye()


brm1$data |>
  distinct(source, year_s) |>
  filter(year_s >= 0.14, year_s <= 0.58
           ) |>
  add_epred_draws(brm1, 
                  re_formula = NA, ndraws = 1000) |>
  pivot_wider(id_cols = c(source, .draw), names_from = year_s, values_from = .epred) |>
  mutate(prior = rnorm(n(), mean = 0, sd = 0.5),
         b_year = `0.579024981657999` - `0.142937036414116`) |>
  pivot_longer(prior:b_year) |>
  ggplot(aes(x = value, 
             y = source, 
             fill = name)) +
  stat_halfeye() +
  xlim(-.2, .2)




data.frame(
  year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(brm1$data$source)) |>
  add_epred_draws(brm1,
                  re_formula = NA,
                  allow_new_levels = TRUE) |>
  ungroup()|>
  mutate(.epred = .epred * max_temp) |>
  select(-.row, -.chain, -.iteration, -year_s) |>
  pivot_wider(names_from = "year_real",
              values_from = ".epred") |>
  mutate(slope_new = `2020` - `2019`) |>
  # slope_new = chnage in actual temperature right scale
  mutate(prop = slope_new / `2019` * 100,
         prior = exp(rnorm(n(), mean = 0, sd = 0.5))) |>
  pivot_longer(c(slope_new, prior)) |>
  ggplot(aes(x = value, 
             y = source, 
             fill = name)) +
  stat_halfeye()
  


data.frame(
  year_real = c(2019, 2020)) |>
  mutate(year_s = (year_real - mean_year)/sd_year) |>
  expand_grid(source = unique(brm1$data$source)) |>
  add_epred_draws(brm1,
                  re_formula = NA,
                  allow_new_levels = TRUE) |>
  ungroup()|>
  mutate(.epred = .epred * max_temp) |>
  select(-.row, -.chain, -.iteration, -year_s) |>
  pivot_wider(names_from = "year_real",
              values_from = ".epred") |>
  mutate(slope_new = `2020` - `2019`) |>
  # slope_new = chnage in actual temperature right scale
  mutate(prop = slope_new / `2019` * 100) |>
  ggplot(aes(x = prop, 
             y = source, 
             fill = source)) +
  stat_halfeye()


prior_draws(brm1) |> 
  names()
spread_draws(brm1, b_year_s)
spread_draws(brm1, shape)

prior_draws(brm1) |>
  mutate(b_year_s = spread_draws(brm1, b_year_s, ndraws = 1000)$b_year_s) |>
  select(b, b_year_s) |>
  rename(prior = b, posterior = b_year_s) |>
  pivot_longer(prior:posterior) |>
  ggplot(aes(x = value, 
             fill = name)) +
  stat_halfeye() +
  labs(title = "Year coefficient")

prior_draws(brm1) |>
  mutate(Intercept = spread_draws(brm1, Intercept, ndraws = 1000)$Intercept) |>
  select(b, Intercept) |>
  rename(prior = b, posterior = Intercept) |>
  pivot_longer(prior:posterior) |>
  ggplot(aes(x = value, 
             fill = name)) +
  stat_halfeye(alpha = 0.75) +
  labs(title = "Intercept coefficient")
