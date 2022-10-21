#Set your home directory below:
working_directory <- "~/Personal_projects/EPL_2022_2023//"
working_directory <- "C:/Users/Simon/OneDrive - nd.edu/Documents/Personal_projects/EPL_2022_2023/"
setwd(working_directory)

#packages
library(sparkline)
library(xfun)
library(htmltools)
library(webshot) 
library(rvest)
library(stringi)
library(stringr)
library(formattable)
library(dplyr)
library(tidyr)
library(ggplot2)

#test the import
source("import_test.R")
import_test()

#number of simulations (can change this... 1000 takes about 3 minutes):
simulated_seasons <- 1000

#home field advantage (multiplied by the home teams offense. default is 1.2)
home_field_advantage <- 1.20

#global variables (don't change)
teams_n <- 20
matchweeks_total <- 38


#Create Functions
source("create_table.R")
source("populate_wins_losses.R")
source("simulate_game.R")
source("simulate_season.R")
source("simulate_many_seasons.R")

#import SPI
source("import_SPI.R")

#import scores
source("import_scores_and_schedule.R")

#create final table
source("set_up_final_table.R")

#Display table
show(table_out)

#Save table as png in the table_output folder for posterity
path <- html_print(table_out, background = "white", viewer = NULL)
url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
webshot(url,
        file = paste0("table_output/Matchweek_", current_matchday, "_EPL_table.png"),
        selector = ".formattable_widget",
        delay = 0.2)

#update the current file:
path <- html_print(table_out, background = "white", viewer = NULL)
url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
webshot(url,
        file = paste0("table_output/current_EPL_table.png"),
        selector = ".formattable_widget",
        delay = 0.2)


#Save SPI tables as CSV document
write.table(SPI_df, paste0("SPI_tables/Matchweek_", current_matchday, "_SPI.csv"),
            sep = ",", row.names = F)

#save current table as CSV document
current_table_output <- subset(current_table, select = -(Pos_trend))
write.table(current_table_output, paste0("current_table_output/Matchweek_", current_matchday, "_table.csv"),
            sep = ",", row.names = F)


source("weekly_analysis.R")
source("weekly_analyisis_SPI.R")


