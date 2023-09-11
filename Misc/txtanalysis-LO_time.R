library(readxl)
library(stringr)
library(tidyverse)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

source('learningGoalsTextAnalysis.R')

testfile <-
  LearningGoalsDataset

testfile <-
  filter(testfile, testfile$'Academic program'=="W")

str = paste(testfile$LOs, sep='')
str <- toString(str, sep='')
str <- gsub(",", "", str)
str <- gsub(" ", "", str)
str <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', str))
str <- strsplit(str, " ")
#LearningGoalsDataset$'Academic program'[2]

str <- testfile$LOs

docs <- Corpus(VectorSource(str))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


View(testfile)
