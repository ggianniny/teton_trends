library(tidyverse)
library(brms)
library(tidybayes)

# where did site_temps come from???

site_temps |>
  #ungroup() |>
  select(site, year, .temp, .draw, source) |>
  filter(year %in% c(2018, 2024)) |>
  filter(site %in% c("grizzly", "s_cascade", "mid_teton", "delta")) |>
  group_by(source, site) |>
  mutate(year_cat = case_when(
    year == min(year) ~ "start", 
    .default = "end")) |>
  distinct(site, year_cat, year) |>
  pivot_wider(id_cols = c(site, source),
              names_from = year_cat,
              values_from = year) |>
  mutate(year_range = end - start) |>
  arrange(start, end)




# last year - first year
brm1$data |>
  select(source, year_s, site) |>
  distinct() |>
  group_by(site) |>
  filter(year_s == min(year_s) |
           year_s == max(year_s)) |>
  mutate(year = (year_s*sd_year)+mean_year) |>
  add_epred_draws(
    brm1,
    #allow_new_levels = TRUE,
    re_formula = NULL) |> # includes random effects
  mutate(.temp = (.epred*max_temp)) |>
  ungroup() |>
  select(site, year, .temp, .draw, source) |>
  group_by(source, site, .draw) |>
  mutate(year_cat = case_when(
    year == min(year) ~ "start", 
    .default = "end")) |>
  select(-year) |>
  pivot_wider(id_cols = c(site, .draw, source),
              names_from = year_cat,
              values_from = .temp) |>
  mutate(delta = end - start,
         group = cur_group_id()) |>
  ggplot(aes(y = fct_reorder(site, group), 
             x = delta, 
             fill = source)) +
  stat_halfeye()+
  scale_fill_manual(values = c("deepskyblue",
                               "slategrey", 
                               "springgreen4")) +
  theme_bw() +
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  labs(y = "site",
       x = "Absolute change in average August temperatures")


brm1$data |>
  select(source, site) |>
  distinct() |>
  expand_grid(year = c(2018, 2024)) |>
  mutate(year_s = (year - mean_year) / sd_year) |>
  add_epred_draws(
    brm1,
    #allow_new_levels = TRUE,
    re_formula = NULL) |> # includes random effects
  mutate(.temp = (.epred*max_temp)) |>
  ungroup() |>
  select(site, year, .temp, .draw, source) |>
  group_by(source, site, .draw) |>
  mutate(year_cat = case_when(
    year == min(year) ~ "start", 
    .default = "end")) |>
  select(-year) |>
  pivot_wider(id_cols = c(site, .draw, source),
              names_from = year_cat,
              values_from = .temp) |>
  mutate(delta = end - start) |>
  group_by(site) |>
  mean_qi(delta) |>
  arrange(delta) |>
  filter(site %in% c("grizzly", "mid_teton", "delta", "s_cascade"))



brm1$data |>
  select(source, year_s, site) |>
  distinct() |>
  group_by(site) |>
  filter(year_s == min(year_s) |
           year_s == max(year_s)) |>
  mutate(year = (year_s*sd_year)+mean_year) |>
  add_epred_draws(
    brm1,
    re_formula = NULL) |> # includes random effects
  mutate(.temp = (.epred*max_temp)) |>
  ungroup() |>
  select(site, year, .temp, .draw, source) |>
  group_by(source, site, .draw) |>
  mutate(year_cat = case_when(
    year == min(year) ~ "start", 
    .default = "end")) |>
  select(-year) |>
  pivot_wider(id_cols = c(site, .draw, source),
              names_from = year_cat,
              values_from = .temp) |>
  mutate(delta = end - start,
         group = cur_group_id()) |>
  group_by(site) |>
  mean_qi(delta) |>
  arrange(delta) |>
  filter(site %in% c("grizzly", "mid_teton", "delta", "s_cascade"))
