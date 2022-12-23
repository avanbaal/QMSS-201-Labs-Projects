# Project: Word Cloud Slotkin's Tweets" 
# Author: Andrew van Baal
# Data: Scraped from candidate acct
##########################################################
#load tidyverse
library(package = "tidyverse")

# bring in sample dataset
Slotkindata <- read.csv(file.choose(), header = T)

#look at data
Slotkindata

#see how R read the data and what data types were assigned
summary(Slotkindata)

#Build Corpus
library (package ="tm")
library (package = "SnowballC")

SlotkinCorpus<- Corpus(VectorSource(Slotkindata$Tweet.Text))

#CLEAN CORPUS DATA

#****Alternative Code*****
#Junge <- Junge %>%
#  tm_map(removeNumbers) %>%
#  tm_map(removePunctuation) %>%
#  tm_map(stripWhitespace)
#Junge <- tm_map(Junge, content_transformer(tolower))
#Junge <- tm_map(Junge, removeWords, stopwords("english"))


# converts all text to lower case
SlotkinCorpus = tm_map(SlotkinCorpus, tolower) 
inspect (SlotkinCorpus [1:5])

#Removes Punctuation
SlotkinCorpus = tm_map(SlotkinCorpus, removePunctuation) 
inspect (SlotkinCorpus [1:5])

#Removes common english words
SlotkinCorpus = tm_map(SlotkinCorpus, removeWords, stopwords('english'))
inspect (SlotkinCorpus[1:5])

#Transforms to root words
SlotkinCorpus = tm_map(SlotkinCorpus, stemDocument) 
inspect (SlotkinCorpus[1:5])

#takes out https
removeURL <- function (x) gsub('http[[:alnum:]]*','', x)
SlotkinCorpus <- tm_map(SlotkinCorpus, content_transformer(removeURL))
inspect(SlotkinCorpus[1:5])

#takes out spaces left by removing previous misc.
SlotkinCorpus <- tm_map (SlotkinCorpus, stripWhitespace)
inspect (SlotkinCorpus[1:5])

#Make TDM -> Term Document Matrix - unstructured becomes structured with rows and columns counting words
Slotkintdm <- TermDocumentMatrix(SlotkinCorpus)
Slotkintdm

Slotkintdm <- as.matrix(Slotkintdm)
Slotkintdm[1:10, 1:20]

#See freq of words, then exclude to only words showing more than 5 times
eachword <- rowSums(Slotkintdm)
eachword

subofeach <-subset(eachword, eachword>=5)
subofeach

#barplot with words typed vertically
barplot (subofeach, las=2)

#make a word cloud
install.packages("wordcloud")
library(wordcloud)

install.packages("RColorBrewer")
library(RColorBrewer)

SlotkinCloud <- sort(rowSums(Slotkintdm),decreasing=TRUE)
set.seed (123)
wordcloud (words=names(subofeach),
           freq=subofeach,
           max.words=30,
           colors=brewer.pal(8, "Dark2"))
#_________________________________________________________________
#Sentiment Analysis - install these and then just read the library
library(syuzhet)
library (lubridate)
library (ggplot2)
library(scales)
library (reshape2)
library (dbplyr)

#Read in the file then read only the tweets column
# bring in Junge dataset
PJtweet.data <- read.csv(file.choose(), header = T)

summary (PJtweet.data)

Slotkinonly <- iconv(Slotkindata$Tweet.Text)
PJtweetsonly <- iconv(PJtweet.data$Tweet.Text)

#Get Sentiment Scores

scores <- get_nrc_sentiment(Slotkinonly)
head (scores)

Slotkinonly[4]
PJtweetsonly[1]
get_nrc_sentiment('thank')
get_nrc_sentiment('president')

barplot (colSums(scores),
         las=2,
         col=rainbow(10),
         ylab="counts",
         main="Sentiment Score for Elissa Slotkin Tweets")
