# Load ggplot2
library(ggplot2)
library(scales)
library(rstatix)
library(forcats)
library(gridExtra)
library(zoo)
library(ggforce)
library(dplyr)
library(lubridate)

# Read in the data
setwd('/Users/scotthotaling/Desktop/TASR Temp Analysis/')
pb_data <- read.csv("paintbrush_data.csv")

# Make plots
above_20 <- ggplot(pb_data, aes(x=year, y=more_than_20)) + geom_point() + geom_smooth(method="lm")
above_20

first_day <- ggplot(pb_data, aes(x=year, y=first_day)) + geom_point() + geom_smooth(method="lm")
first_day

taxa <- ggplot(pb_data, aes(x=year, y=taxa)) + geom_point() + geom_smooth(method="lm")
taxa

# Put plots together
grid.arrange(above_20, first_day, taxa, ncol = 1, nrow = 3)
