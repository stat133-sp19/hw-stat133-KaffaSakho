  #title: Data Preparation
  #description: This document contains all the R code used to create a csv data file containing the required variables that will be used in the visualisation phase."
  #input(s): Andre Iguodala Data Set, Draymond Green Data Set, Kevin Durant Data Set, Stephen Curry Data Set, Klay Thompson Data Set"
  #output(s): Data Frames of Each Player, Text Files of the Summaries of Each Player's Data Frames, a CSV file of a Stacked table of all Players Data, a Text File of the Summary of the Stacked table
  
    library(dplyr)
  
  setwd("/Users/AKaffa/Desktop/workout01")
  
  #Read Iguodala's Data Set 
  iguodala <- read.csv("data/andre-iguodala.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","character", "character","character","double","character","double","double"), sep=",")
  iguodala <- mutate(iguodala, name = "Andre Iguodala")
  
  #Read Green's Data Set 
  green <- read.csv("data/draymond-green.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","character", "character","character","double","character","double","double"), sep=",")
  green <- mutate(green, name = "Draymond Green")
  
  #Read Durant Data Set 
  durant <- read.csv("data/kevin-durant.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","character", "character","character","double","character","double","double"), sep=",")
  durant <- mutate(durant, name = "Kevin Durant")
  
  #Read Thompson Data Set 
  thompson <- read.csv("data/klay-thompson.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","character", "character","character","double","character","double","double"), sep=",")
  thompson <- mutate(thompson, name = "Klay Thompson")
  
  #Read Curry's Data Set 
  curry <- read.csv("data/stephen-curry.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","character", "character","character","double","character","double","double"), sep=",")
  curry <- mutate(curry, name = "Stephen Curry")
  
  #Changing Shot_made_flag
  iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
  iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
  green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
  green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
  durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
  durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
  thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
  thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
  curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
  curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
  
  #Adding a Column Minute 
  iguodala <- mutate(iguodala, minute= 12*(iguodala$period-1)+ (12-iguodala$minutes_remaining))
  green <- mutate(green, minute= 12*(green$period-1)+ (12-green$minutes_remaining))
  durant <- mutate(durant, minute= 12*(durant$period-1)+ (12-durant$minutes_remaining))
  thompson <- mutate(thompson, minute= 12*(thompson$period-1)+ (12-thompson$minutes_remaining) )
  curry <- mutate(curry, minute= 12*(curry$period-1)+ (12-curry$minutes_remaining))
  
  #Creating Outputs of Imported Data
  sink(file = 'output/andre-iguodala-summary.txt')
  summary(iguodala)
  sink(file = 'output/draymond-green-summary.txt')
  summary(green)
  sink(file = 'output/kevin-durant-summary.txt')
  summary(durant)
  sink(file = 'output/klay-thompson-summary.txt')
  summary(thompson)
  sink(file = 'output/stephen-curry-summary.txt')
  summary(curry)
  
  #Stack the Tables
  single_table <- rbind(iguodala,green,durant,thompson,curry)
  write.csv(single_table, file = 'data/shots-data.csv', row.names=TRUE)
  sink(file = 'data/shots-data.csv')
  (write.csv(single_table, file = 'data/shots-data.csv'))
  
  sink(file = 'output/shots-data-summary.txt')
  summary(single_table)

  