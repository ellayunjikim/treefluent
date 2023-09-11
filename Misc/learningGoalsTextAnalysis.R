library(readxl)
library(stringr)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library("SnowballC")
library(ggplot2)

#setwd('C:\\Users\\16032\\OneDrive - Grinnell College\\Desktop\\Year 3 Sem 2\\CSC324\\Treefluent')

LearningGoalsDataset <- read_excel("LearningGoalsDataset.xlsx")
#View(LearningGoalsDataset)

LO_1 = c("creative", "critical", "thinking", "analyze", "other", "quantitative", "qualitative", "problem",
         '1', "critique", "formulate", "question", "understand", "evaluate", "moving", "themes", "draw", "base",
         "interpret")
LO_2 = c("social", "responsibility", "fairness", "equity", '2', "copyright", "honesty", "service", "respect", 
         "integrity", "individual", "rights", "civil", "ethic", "committee", "policy", "policies", "governance")
LO_3 = c("communicate", "persuade", "write", "present", '3', "speak", "purpose", "audience", "respond", "dialogue",
         "discussion", "written", "oral", "comprehend", "argue", "language", "express", "verbal", "writing",
         "debate", "publication", "foreign", "abroad", "fine arts", "ensemble", "production", "french", "arabic",
         "chinese", "japanese", "latin", "german", "russian", "elvish", "spanish", "describe", "create", "talk",
         "vocab")
LO_4 = c("independent", "collaborate", "research", '4', "workshop", "resources", "new areas", "background", 
         "lifelong", "learner", "new skill", "MAP", "seminar", "intern", "fine art", "performance", "athletic", 
         "peer instruct", "peer mentor", "peer tutor", "peer feedback", "govern", "SGA", "SEPC", "daily l", "original")
LO_5 = c("approach", "reason", "perspectives", '5', "multiple", "multi", "disciplinary", "disciplines", "opposing",
         "conflict", "discriminat", "bias", "background", "society", "societal", "global", "diverse", "perspective", 
         "off-campus", "ISO", "internation", "concentration")
LO_6 = c("depth", "inquiry", "field", '6', "independent", "intern", "method", "tool", "technique", "technolog", 
         "techniques", "apply")


LearningGoalsDataset <- rename(LearningGoalsDataset, 'LOBA' = 'Learning outcome being assessed')

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = "")

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_1, collapse = "|"), tolower(LOBA)), 1, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_2, collapse = "|"), tolower(LOBA)), 2, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_3, collapse = "|"), tolower(LOBA)), 3, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_4, collapse = "|"), tolower(LOBA)), 4, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_5, collapse = "|"), tolower(LOBA)), 5, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_6, collapse = "|"), tolower(LOBA)), 6, ""), sep = ""))

LearningGoalsDataset <- 
  LearningGoalsDataset %>%
  mutate(MATCH = FALSE)

LearningGoalsDataset <- rename(LearningGoalsDataset, 'Learning outcome being assessed'='LOBA')

wordnumbs <- ""

for (obs in LearningGoalsDataset$'LOs'){
  nums <- str_split(obs, '')
  for (i in nums){
    wordnumbs <- paste(wordnumbs, i, sep = ' ')
  }
}

words = ''
for (i in wordnumbs){
  words <- paste(words, i)
}

words <- strsplit(words, "\\s+")[[1]]

word <- c('1', '2', '3', '4', '5', '6')
freq <- c(0, 0, 0, 0, 0, 0)


for (i in words){
  if (i=='1'){
    freq[1] <- (freq[1]+1)
  }
  else if (i=='2'){
    freq[2] <- (freq[2]+1)
  }
  else if (i=='3'){
    freq[3] <- (freq[3]+1)
  }
  else if (i=='4'){
    freq[4] <- (freq[4]+1)
  }
  else if (i=='5'){
    freq[5] <- (freq[5]+1)
  }
  else if (i=='6'){
    freq[6] <- (freq[6]+1)
  }
}



df <- data.frame(word, freq)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#work for bar chart
LO=c(1,2,3,4,5,6)
correct=c(0,0,0,0,0,0)
incorrect=c(0,0,0,0,0,0)

for (i in 1:length(LearningGoalsDataset$`Submission Date`)){
  LO_num = strtoi(LearningGoalsDataset$'College-Wide Learning Outcome Num'[i])
  if (grepl(LearningGoalsDataset$'College-Wide Learning Outcome Num'[i], LearningGoalsDataset$LOs[i])){
    LearningGoalsDataset$MATCH[i] <- TRUE
    correct[LO_num] <- (correct[LO_num]+1)
  }
  else {
    LearningGoalsDataset$MATCH[i] <- FALSE
    incorrect[LO_num] <- (incorrect[LO_num]+1)
  }
}

bar_df <- data.frame(LO, correct, incorrect)

bar_df <-
  bar_df %>%
  mutate(percent_correct = correct/(correct+incorrect))

bar_df <-
  bar_df %>%
  mutate(sd = (correct*(1-percent_correct)^2+incorrect*(percent_correct)^2)/(correct+incorrect))

ggplot(bar_df) +
  geom_bar(aes(x=LO, y=percent_correct), stat='identity') +
  geom_errorbar(aes(x=LO, ymin=percent_correct-sd, ymax=percent_correct+sd),
             width=0.4, colour="orange", alpha=0.8, size=1.3)



#View(LearningGoalsDataset)
