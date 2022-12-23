#Lab 9: Intro to R and RStudio
#QMSS 201-005
#By: Andrew van Baal
View(music_data)

#Installing and writing in relevant packages
library(package = "tidyverse")
library(package="janitor")
library(dplyr)

#Running summary and glimpse commands
summary(music_data)
glimpse(music_data)

#Cleaning the data
musicdata.clean <- music_data %>%
  rename(Gender=Sex, Country=`Country Music`, Hours=`Hours per day listening to Music`, Platform=`Favorite Music Streaming Platform`)%>%
  #Changing Gender and Platform to Factors
  mutate(Gender = as.factor(x = Gender))%>% 
  mutate(Platform=as.factor(x = Platform))%>% 
  mutate(Gender=recode(.x=Gender,'1'='Male', '2'='Female'))%>%
  mutate(Platform = recode(.x=Platform, 'Not Sure' = 'NA')) %>%
  #Selecting Variables for clean dataframe
  select(Gender, Country, Hours, Platform)

#Viewing the summary of the cleaned data
summary(musicdata.clean)

#Saving reduced dataframe as a CSV
write.csv(musicdata.clean, "MusicvanBaal.csv", row.names = FALSE)

#The end -- looking forward to learning more about R!!

