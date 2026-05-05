library(tidyverse)

dat <- read_csv("invert_data/cleaned_csv/full_invert_densities.csv")
dat

dat |>
  distinct(stream_code)

dat |>
  filter(stream_code == "SC")

# three KH sites in 2018
dat |>
  filter(stream_code == "GR" | 
           stream_code == "DE" | 
           stream_code == "SC" ,
         Year == 2018) |>
  group_by(stream_code, Year) |>
  summarize(ab = sum(abundance_total),
            density = sum(density_xbar))

# three KH sites for all years
dat |>
  filter(stream_code == "GR" | 
           stream_code == "DE" | 
           stream_code == "SC" ) |>
  group_by(stream_code, Year) |>
  summarize(ab = sum(abundance_total),
            density = sum(density_xbar))

dat |>
  filter(stream_code == "GR",
         Year == 2018) |>
  summarize(sum(abundance_total))
  

dat_raw <- read_csv("invert_data/raw_data/csv/2015-2018_inverts.csv")
dat_raw |>
  select(Stream, Name, Year, Rep, Taxa, Abundance, Density) |>
  filter(Name == "Delta" |
           Name == "Griz",
         Year == 2018) |>
  group_by(Year, Name, Rep) |>
  summarize(s_ab = sum(Abundance),
            s_d = sum(Density)) |>
  mutate(multiplier = s_d / s_ab)

distinct(dat_raw, Name, Stream)
