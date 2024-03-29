library(tidyverse)

plot(density(rnorm(1000, 0, 0.5)))
plot(density(rexp(1000, 2)))

n <- 1000
source <- c("a", "b", "c")
b_year <- rnorm(n, 0, 0.5)
b_source <- rnorm(n, 0, 0.5)
site_sd <- rnorm(n, 0, 0.5)

(df <- expand_grid(
  year = seq(-3, 3, length.out = 8),
  source = source,
  rep = 1:n
) %>%
  as_tibble())

df %>%
  mutate(
    b_year = rnorm(nrow(.), 0, 0.5),
    b_source = rnorm(nrow(.), 0, 0.5),
    site_sd = rnorm(nrow(.), 0, 0.5),
    y = b_year * year + b_source+site_sd) %>%
  ggplot(aes(x = year,
             y = y,
             group = rep,
             color = source)) +
  stat_smooth(method = "lm",
              alpha = 0.5,
              linewidth = 0.5,
              se = FALSE) +
  geom_point() +
  facet_wrap(~source)
