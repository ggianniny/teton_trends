# Packages
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("rstatix")
# install.packages("forcats")
# install.packages("gridExtra")
# install.packages("zoo")
# install.packages("ggforce")
# install.packages("lubridate")

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

# Read in the main data
setwd('/Users/scotthotaling/Desktop/TASR Temp Analysis/')
wind_cave <- read.csv("Wind Cave 2.csv")
middle_teton <- read.csv("Middle Teton.csv")
nf_teton <- read.csv("N F Teton Creek.csv")
ak_basin_south <- read.csv("AK Basin South.csv")
s_cascade_canyon <- read.csv("S Cascade Canyon.csv")

# Check how the data are being interpreted
str(wind_cave)

## Make line plots

# Set colors
colors <- c("2015" = "#BBDEFB", "2016" = "#9FA8DA", "2017" = "#BA68C8", "2018" = "#E57373", "2019" = "#F44336", "2020" = "#D32F2F", "2021" = "#880E4F", "2022" = "#000000")

# Wind Cave
wind_cave_plot <- ggplot(data = wind_cave, aes(x = Order)) +
	geom_line(aes(y = temp_2015, color = "2015", group = 1), size = 1) +
	geom_line(aes(y = temp_2016, color = "2016", group = 1), size = 1) +
	geom_line(aes(y = temp_2017, color = "2017", group = 1), size = 1) +
	geom_line(aes(y = temp_2018, color = "2018", group = 1), size = 1) +
	geom_line(aes(y = temp_2019, color = "2019", group = 1), size = 1) +
	geom_line(aes(y = temp_2020, color = "2020", group = 1), size = 1) +
	geom_line(aes(y = temp_2021, color = "2021", group = 1), size = 1) +
	labs(x = "Date", y = "Temperature (°C)", color = "Legend") +
	theme(legend.position="none", legend.title = element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
	scale_color_manual(values = colors) +
	scale_y_continuous(limits = c(0,14), expand = c(0, 0)) +
	ggtitle("Wind Cave")
	
wind_cave_plot
	

# Middle Teton
middle_teton_plot <- ggplot(data = middle_teton, aes(x = Order)) +
	geom_line(aes(y = temp_2015, color = "2015", group = 1), size = 1) +
	geom_line(aes(y = temp_2016, color = "2016", group = 1), size = 1) +
	geom_line(aes(y = temp_2017, color = "2017", group = 1), size = 1) +
	geom_line(aes(y = temp_2018, color = "2018", group = 1), size = 1) +
	geom_line(aes(y = temp_2019, color = "2019", group = 1), size = 1) +
	geom_line(aes(y = temp_2020, color = "2020", group = 1), size = 1) +
	geom_line(aes(y = temp_2021, color = "2021", group = 1), size = 1) +
	geom_line(aes(y = temp_2022, color = "2022", group = 1), size = 1) +
	labs(x = "Date", y = "Temperature (°C)", color = "Legend") +
	scale_color_manual(values = colors) +
	theme(legend.position = c(0.12, 0.65), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  	scale_y_continuous(limits = c(0,14), expand = c(0, 0)) +
  	ggtitle("Middle Teton")

middle_teton_plot


# N F Teton Creek
nf_teton_plot <- ggplot(data = nf_teton, aes(x = Order)) +
	geom_line(aes(y = temp_2015, color = "2015", group = 1), size = 1) +
	geom_line(aes(y = temp_2016, color = "2016", group = 1), size = 1) +
	geom_line(aes(y = temp_2017, color = "2017", group = 1), size = 1) +
	geom_line(aes(y = temp_2018, color = "2018", group = 1), size = 1) +
	geom_line(aes(y = temp_2019, color = "2019", group = 1), size = 1) +
	geom_line(aes(y = temp_2020, color = "2020", group = 1), size = 1) +
	geom_line(aes(y = temp_2021, color = "2021", group = 1), size = 1) +
	geom_line(aes(y = temp_2022, color = "2022", group = 1), size = 1) +
	labs(x = "Date", y = "Temperature (°C)", color = "Legend") +
	theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
	scale_color_manual(values = colors) +
  	scale_y_continuous(limits = c(0,14), expand = c(0, 0)) +
	ggtitle("N.F. Teton Creek")

nf_teton_plot


# AK Basin South
ak_basin_south_plot <- ggplot(data = ak_basin_south, aes(x = Order)) +
	geom_line(aes(y = temp_2016, color = "2016", group = 1), size = 1) +
	geom_line(aes(y = temp_2019, color = "2019", group = 1), size = 1) +
	geom_line(aes(y = temp_2020, color = "2020", group = 1), size = 1) +
	geom_line(aes(y = temp_2021, color = "2021", group = 1), size = 1) +
	geom_line(aes(y = temp_2022, color = "2022", group = 1), size = 1) +
	labs(x = "Date", y = "Temperature (°C)", color = "Legend") +
	scale_color_manual(values = colors) +
  	theme(legend.position="none", axis.title.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
	scale_y_continuous(limits = c(0,8), expand = c(0, 0)) +
	ggtitle("Alaska Basin South")

ak_basin_south_plot



# S Cascade Canyon	
s_cascade_canyon_plot <- ggplot(data = s_cascade_canyon, aes(x = Order)) +
	geom_line(aes(y = temp_2015, color = "2015", group = 1), size = 1) +
	geom_line(aes(y = temp_2016, color = "2016", group = 1), size = 1) +
	geom_line(aes(y = temp_2020, color = "2020", group = 1), size = 1) +
	geom_line(aes(y = temp_2021, color = "2021", group = 1), size = 1) +
	geom_line(aes(y = temp_2022, color = "2022", group = 1), size = 1) +
	labs(x = "Date", y = "Temperature (°C)", color = "Legend") +
	scale_color_manual(values = colors) +
	theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  	scale_y_continuous(limits = c(0,14), expand = c(0, 0))+
  	ggtitle("South Cascade")
	
s_cascade_canyon_plot


### Put combined temp plot together
grid.arrange(wind_cave_plot, middle_teton_plot, nf_teton_plot, s_cascade_canyon_plot, ncol = 2, nrow = 2)


	