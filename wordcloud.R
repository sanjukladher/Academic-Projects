library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(sqldf)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(tm)

hr <- read.csv("Hotel_Reviews.csv", header = T)

head(hr)


reviews <- hr[sample(nrow(hr), 40000), ]
reviews <- reviews[reviews$Positive_Review!='No Positive',]
reviews <- reviews[reviews$Negative_Review!='No Negative',]
term_freq <- function(hr,sent){
  if(sent=='pos'){
    corpus <- Corpus(VectorSource(hr$Positive_Review))
  }else{
    corpus <- Corpus(VectorSource(hr$Negative_Review))
  }
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <-TermDocumentMatrix(corpus)
  mat_dtm <- as.matrix(dtm)
  v_dtm <- sort(rowSums(mat_dtm),decreasing = TRUE)
  FreqMat <- data.frame(word = names(v_dtm), Freq = v_dtm)
  FreqMat <- FreqMat[1:100,]
  return(FreqMat)
}

wordcloud2(data,minRotation = 0,maxRotation = 0, size =4,shape = "diamond")

wordcloud2(data = term_freq(reviews,'neg'),minRotation = 0,maxRotation = 0)




reviews <- hr[sample(nrow(hr), 40000), ]
reviews <- reviews[reviews$Reviewer_Nationality!=' ',]

term_freq <- function(hr){
  corpus <- Corpus(VectorSource(hr$Reviewer_Nationality))
  dtm <-TermDocumentMatrix(corpus)
  mat_dtm <- as.matrix(dtm)
  v_dtm <- sort(rowSums(mat_dtm),decreasing = TRUE)
  FreqMat <- data.frame(word = names(v_dtm), Freq = v_dtm)
  return(FreqMat)
}

data = term_freq(reviews)
data
wordcloud2(data,size =10,fontWeight = 80,minSize = 5)
