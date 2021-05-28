getwd()
setwd("/Users/renachoi/Desktop/hackathon/")
#install.packages("tm")
#install.packages("NLP")
#install.packages("syuzhet")
#install.packages("lubridate")
#install.packages("tidyverse")
install.packages("pillar")
install.packages("devtools")

library(NLP)
library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(pillar)
library(devtools)

claimfake_tweets <- read.csv(file="ClaimFake_tweets.csv", header = TRUE)
claimfake_tweets_r<- read.csv(file="ClaimFake_tweets_replies.csv", header = TRUE)
claimreal_tweets <- read.csv(file="ClaimReal_tweets.csv", header = TRUE)
claimreal_tweets_r<- read.csv(file="ClaimReal_tweets_replies.csv", header = TRUE)
newsfake_tweets <- read.csv(file="NewsFake_tweets.csv", header = TRUE)
newsfake_tweets_r<- read.csv(file="NewsFake_tweets_replies.csv", header = TRUE)
newsreal_tweets <- read.csv(file="NewsReal_tweets.csv", header = TRUE)
newsreal_tweets_r<- read.csv(file="NewsReal_tweets_replies.csv", header = TRUE)

str(claimfake_tweets)

#build corpus
corpus <- iconv(claimfake_tweets_r$text, to = 'utf-8-mac')
corpus <- VCorpus(VectorSource(corpus))
inspect(corpus[1:2])

#clean text
corpus <- tm_map(corpus, content_transformer(tolower)) #change to lower case
corpus <- tm_map(corpus, removePunctuation) #remove all punctation
corpus <- tm_map(corpus,removeNumbers) #remove all number
corpus <- tm_map(corpus, removeWords, stopwords('english'))
#removeURL <- function(x) gsub('http[[:alnum:]]*',' ', x)
#corpus <- tm_map(corpus, content_transformer(removeURL))
#inspect(cleanset[1:2])

corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,stemDocument)
corpus <- tm_map(corpus, removeWords, c('just','like'))

#term document matrix
tdm <- TermDocumentMatrix(corpus)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:30]

#bar plot
w <- rowSums(tdm)
w <- subset(w,w>=25) #only includes that showing up more than 25 times
barplot(w,
        las = 2,
        col = rainbow(6),
        ylim = c(0,80))

#word cloud
#install.packages("wordcloud")
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
w <- sort(rowSums(tdm), decreasing = T)
set.seed(222)
wordcloud(words=names(w),freq=w,
          random.order =F,
          min.freq = 3,
          colors = brewer.pal(8,'Dark2'),
          scale = c(7,0.3),
          rot.per = 0.7)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           size = 0.8,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(w,
            word = "COVID",
            size=1.5)

#Sentiment analysis

#readfile
tweets <- iconv(claimfake_tweets_r$text, to = 'utf-8-mac')

#obtain sentiment scores
s <- get_nrc_sentiment(tweets)
get_nrc_sentiment('covid')
get_nrc_sentiment('flu')

#bar plot
barplot(colSums(s),
        las= 2,
        col = rainbow(10),
        ylab = 'count',
        main = 'Sentiment Scores for COVID Tweets')