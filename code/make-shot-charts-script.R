#title: 
#description: "This file contains the charts that will be used in the report."
#inputs: 
#outputs:
  
  #4.1) Shot Charts of Each Player
  
library(ggplot2)
library(dplyr)
library(jpeg)
library(grid)
court_file <- "images/nba-court.jpg"
court_image <- rasterGrob(readJPEG(court_file), width= unit(1,"npc"), height= unit(1,"npc"))

#Creating Data Frames
iguodala <- read.csv("data/andre-iguodala.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","factor","character","character","double","character","double","double"))
iguodala <- mutate(iguodala, name = "Andre Iguodala")

green <- read.csv("data/draymond-green.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","factor","character","character","double","character","double","double"))
green <- mutate(green, name = "Draymond Green")

durant <- read.csv("data/kevin-durant.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","factor", "character","character","double","character","double","double"))
durant <- mutate(durant, name = "Kevin Durant")

thompson <- read.csv("data/klay-thompson.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","factor", "character","character","double","character","double","double"))
thompson <- mutate(thompson, name = "Klay Thompson")

curry <- read.csv("data/stephen-curry.csv", stringsAsFactors=FALSE, colClasses=c("character","character","integer","integer","integer","integer","factor", "character","character","double","character","double","double"))
curry <- mutate(curry, name = "Stephen Curry")

#Iguodala Shot Chart 
andre_iguodala_shot_chart <- ggplot(data=iguodala)+ 
  annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag)) + 
  ylim(-50,420) + 
  ggtitle('Shot Chart: Andre Iguodala (2016 Season)') + theme_minimal() 

ggsave(filename="images/andre_iguodala_shot_chart.pdf", width=6.5,height=5)

#Green Shot Chart 
draymond_green_shot_chart <- ggplot(data=green)+ 
  annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag)) + 
  ylim(-50,420) + 
  ggtitle('Shot Chart: Draymond Green (2016 Season)') + theme_minimal() 

ggsave(filename="images/draymond_green_shot_chart.pdf", width=6.5,height=5)

#Durant Shot Chart
kevin_durant_shot_chart <- ggplot(data=durant)+ 
  annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag)) + 
  ylim(-50,420) + 
  ggtitle('Shot Chart: Kevin Durant (2016 Season)') + theme_minimal() 

ggsave(filename="images/kevin_durant_shot_chart.pdf", width=6.5,height=5)

#Thompson Shot Chart
klay_thompson_shot_chart <- ggplot(data=thompson)+ 
  annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag)) + 
  ylim(-50,420) + 
  ggtitle('Shot Chart: Klay Thompson (2016 Season)') + theme_minimal() 

ggsave(filename="images/klay_thompson_shot_chart.pdf", width=6.5,height=5)

#Curry Shot Chart
stephen_curry_shot_chart <- ggplot(data=curry)+ 
  annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag, size=3)) + 
  ylim(-50,420) + 
  ggtitle('Shot Chart: Stephen Curry (2016 Season)') + theme_minimal() 

ggsave(filename="images/stephen_curry_shot_chart.pdf", width=6.5,height=5)

#4.2) Facetted Shot Chart
single_table <- rbind(iguodala,green,durant,thompson,curry)

gsw_shot_charts <- ggplot(data=single_table) + annotation_custom(court_image, -250,250,-50,420) + 
  geom_point(aes(x=x,y=y, color=shot_made_flag, size=4)) + 
  ylim(-50,420) + 
  ggtitle('Shot Charts: GSW (2016 Season)') + theme_minimal() + facet_grid(. ~name)

ggsave(filename="images/gsw_shot_charts.pdf", width=8,height=7)
ggsave(filename="images/gsw_shot_charts.png", width=8,height=7)

