###########################################
# Project: Week 11 Lab 
# Purpose: Cleaning + Regressions in R
# Author: Andrew van Baal
# Edit date: Nov 15, 2022
# Data: MI State of the State Fall 2019
###########################################

#Step 2: Reading in Dataset (I chose a more relevant name)
library(readr)
mistatedata<- read.csv('Desktop/22-23/QMSS 201/LABS/Lab 11/soss78wtInclusion Clean race mobility.csv', header=TRUE)
#Step 3: Cleaning the dataset
  #3a: installing and loading 'car' package
install.packages('car')
library(car)
  #3b: New Gender variable
mistatedata$gender <- recode(mistatedata$CD1, "1= 'Male'; 2= 'Female'")
  #3c: Using table to check successful coding of CD1 to gender
genderprop <- table(mistatedata$gender)
table(mistatedata$CD1)
  #3d: Using class to check data types
class(mistatedata$gender)
class(mistatedata$CD1)

#Step 4: Running descriptive statistics on IVs in prep for regression
  #4a: proportion of respondents by gender
prop1 <- prop.table(genderprop)
prop1
  #4b: using summary to find SS for age of respondents
summary(mistatedata$age)

#Step 5: Linear regressions to predict beliefs based on age & gender
lm <- lm(mistatedata$inclusion6 ~ mistatedata$age + mistatedata$gender, data=mistatedata)
summary(lm)

#Step 7: cleaning for binomial logistic regression
  #7a: labor variable & recoding
mistatedata$labor <- recode(mistatedata$laborforce, "1= '1'; 2= '0'")
  #7b: checking class of labor
class(mistatedata$labor)
  #7c changing data type to factor
mistatedata$labor <- as.factor(mistatedata$labor)
  #7d: checking counts
table(mistatedata$labor)
table(mistatedata$laborforce)

#Step 8: binomial logistic regression
glm <- glm(mistatedata$labor ~ mistatedata$age + mistatedata$gender, family = binomial("logit"))
  #8a: summary
summary(glm)

#Step 9: Odds ratios
  #9a: installing odds.n.ends
install.packages('odds.n.ends')
library(odds.n.ends)
  #9b: using on logistic regression to finds odds ratios
odds.n.ends(glm)
contrast(mistatedata$labor)

