## README
### Gordon Gianniny
### Updated 04/21/2023

This project contains code for cleaning and analyzing stream temperature data and invertebrate data from Grand Teton National Park. The general project structure is as follows: 


**Temperature Data** is stored in the "Temperature" folder. The .csv temperature files used for data cleaning and analysis are in "Temperature>raw_temp_data>all_years".

**Temperature Data Cleaning** is executed in the "teton_data_cleaning.Rmd" Rmarkdown script. This script pulls in the raw temperature data above along with the "snotel_airtemps.csv" file in the main project directory and the "deployment_dates.csv" file stored in the "Temperature" folder for data cleaning. Cleaned output files are stored in "Temperature>cleaned_full_datasets". 

**Temperature Data Analysis** is executed in the "teton_temp_trends.Rmd" Rmarkdown script. This script currently performs some basic linear modeling of average summer temperatures from main monitoring sites using the files in the "Temperature>cleaned_csv" folder. The script also runs models including SNOTEL data from the "teton_snotel.csv" file in the project directory. 

**Plots** From temperature data cleaning and analysis are stored in the "Temperature>plots" folder. 
