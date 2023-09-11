#Authors: Colin Finke, Divyansh Singh, Eamon Worden, Ella Kim, Harry MacArthur
#Date: May 11, 2022
#Purpose: Creating visualizations to help understand student engagement and assess departments learning outcomes in comparison to college wide learning outcomes

library(shiny)
library(datasets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(readxl)
library(collapsibleTree)
library(data.tree)
library(DiagrammeR)
library(vtree)
library(tidyverse)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library("SnowballC")
library(plotly)

#preprocessing for Q2
WCLGD <- LearningGoalsDataset_2_
dataset<- CourseData
LearningGoalsDataset <- LearningGoalsDataset_2_

#preprocessing for Q2
#list of keywords for each learning outcome
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

#reformatting
WCLGD <- rename(WCLGD, 'LOBA' = 'Learning outcome being assessed')
#generating LO using keywords above
WCLGD <- 
  WCLGD %>%
  mutate(LOs = "")

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_1, collapse = "|"), tolower(LOBA)), 1, ""), sep = ""))

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_2, collapse = "|"), tolower(LOBA)), 2, ""), sep = ""))

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_3, collapse = "|"), tolower(LOBA)), 3, ""), sep = ""))

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_4, collapse = "|"), tolower(LOBA)), 4, ""), sep = ""))

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_5, collapse = "|"), tolower(LOBA)), 5, ""), sep = ""))

WCLGD <- 
  WCLGD %>%
  mutate(LOs = paste(LOs, ifelse(grepl(paste(LO_6, collapse = "|"), tolower(LOBA)), 6, ""), sep = ""))

#counting number of matches between professor selected LO and our generated LO
WCLGD <- 
  WCLGD %>%
  mutate(MATCH = FALSE)

true=0
false=0

for (i in 1:length(WCLGD$`Submission Date`)){
  if (grepl(WCLGD$'College-Wide Learning Outcome Num'[i], WCLGD$LOs[i])){
    WCLGD$MATCH[i] <- TRUE
    true <- true + 1
  }
  else {
    WCLGD$MATCH[i] <- FALSE
    false <- false + 1
  }
}



#reformatting
WCLGD <- rename(WCLGD, 'LO_Num'='College-Wide Learning Outcome Num')
WCLGD <- rename(WCLGD, 'Program'='Academic program')

set.seed(1234)
#end preprocessing for Q2

server <- function(input, output) {
  #plot for visualization 1
  output$plotman <- renderPlotly({
    #changing color scheme of graph
    col <- input$Color
    #filtering data from user input
    data <- filter(dataset, LO_Num %in% input$LO,
                   Program %in% input$Program,
                   Test_Year >= input$years[1],
                   Test_Year <= input$years[2])
    fv <- data %>% lm(Test_Perc ~ Test_Year,.) %>% fitted.values()
    
    #creating scatterplot
    plot_ly(data,
            x = ~Test_Year,
            y = ~Test_Perc,
            type = 'scatter',
            mode = 'markers',
            text = ~paste('Program: ', Program, ' \nLearning Outcome: ', LO_Num),
            color = ~Test_Perc,
            colors = col
    ) %>% #changing layout of plot
      layout(paper_bgcolor='#c4c4c4',
             plot_bgcolor='#c4c4c4',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage')) %>%
      add_trace(x = ~Test_Year, y = fv, mode = "lines", color = "col") %>%
      layout(showlegend = F)
  })
  
  
  output$vTree1 <- renderVtree({
    vtree(LearningGoalsDataset, "CWLO Course Specific_LO",horiz=FALSE,showcount=FALSE,showpct= FALSE)
  })
  
  output$distPlot <- renderPlot({ 
    ggplot(data = dataset) +
      geom_label(mapping = aes_string(x = input$select, 
                                      y = input$select2,
                                      label = "LO_Num",
                                      color = "Percent_learned"
      ),
      size = 4.5,
      ) + 
      ggtitle(paste("Scatterplot of", input$select, "vs.", input$select2, sep = " ")) +
      scale_colour_gradient2(midpoint = 0.65, low = input$col3, mid = input$col2, high = input$col1)
  })
  
  output$plot1 <- renderCollapsibleTree({
    collapsibleTree(
      LearningGoalsDataset,
      hierarchy = c("Program Group", "Academic program", "Course Num","CWLO","Specific_LO"),
      fill = c(
        # The root
        "seashell",
        # Unique department group
        rep("brown", length(unique(LearningGoalsDataset$"Program Group"))),
        # Unique department per group
        rep("khaki", length(unique(LearningGoalsDataset$"Academic program"))),
        # Unique classes per deprtment
        rep("forestgreen", length(unique(paste(LearningGoalsDataset$"Course Num", LearningGoalsDataset$"Academic program")))),
        # Unique learning outcomes per classes
        rep("blue", length(paste(LearningGoalsDataset$"CWLO", LearningGoalsDataset$"Course Num"))),
        rep("black", length(paste(LearningGoalsDataset$"Specific_LO", LearningGoalsDataset$"Course Num")))
      ),
      linkLength = 100,
      width = NULL,
      height = NULL,
      zoomable = FALSE,
      fillByLevel = TRUE,
      maxPercent = 50
    )
  })
  
  output$wordcloud <- renderPlot({
    #filtering by selected program(s)
    data <- filter(WCLGD, Program %in% input$programs)
    #formatting data
    wordnumbs <- ""
    
    for (obs in data$'LOs'){
      nums <- str_split(obs, '')
      for (i in nums){
        wordnumbs <- paste(wordnumbs, i, sep = ' ')
      }
    }
    #creating a list of all the numbers that appear (in our generated LO lists)
    words = ''
    for (i in wordnumbs){
      words <- paste(words, i)
    }
    
    words <- strsplit(words, "\\s+")[[1]]
    #matrix for creating word cloud
    word <- c('1', '2', '3', '4', '5', '6')
    freq <- c(0, 0, 0, 0, 0, 0)
    
    #counting number of appearances of each learning outcome
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
    #generating data fram
    df <- data.frame(word, freq)
    #creating word cloud
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,
              scale=c(6,10), random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$barchart <- renderPlot({
    #work for bar chart
    #filtering data
    data <- filter(WCLGD, Program %in% input$programs)
    LO=c(1,2,3,4,5,6)
    #setting up to count matches and non matches for each LO
    correct=c(0,0,0,0,0,0)
    incorrect=c(0,0,0,0,0,0)
    
    #checking if the professor selected LO matches one of our generated LOs, adding 1 to correct if it does else adding 1 to incorrect
    for (i in 1:length(data$`Submission Date`)){
      LO_Num = strtoi(data$'LO_Num'[i])
      if (grepl(data$'LO_Num'[i], data$LOs[i])){
        data$MATCH[i] <- TRUE
        correct[LO_Num] <- (correct[LO_Num]+1)
      }
      else {
        data$MATCH[i] <- FALSE
        incorrect[LO_Num] <- (incorrect[LO_Num]+1)
      }
    }
    #creating data frame for number of correct and incorrect matches for each LO
    bar_df <- data.frame(LO, correct, incorrect)
    #generating percent correct
    bar_df <-
      bar_df %>%
      mutate(percent_correct = correct/(correct+incorrect))
    #generating standard deviation
    bar_df <-
      bar_df %>%
      mutate(sd = (correct*(1-percent_correct)^2+incorrect*(percent_correct)^2)/(correct+incorrect))
    #creating a bar chart with error bars based on standard devation for % correct
    ggplot(bar_df) +
      geom_bar(aes(x=LO, y=percent_correct), stat='identity') +
      geom_errorbar(aes(x=LO, ymin=percent_correct-sd, ymax=percent_correct+sd),
                    width=0.4, colour="orange", alpha=0.8, size=1.3) +
      scale_x_continuous(n.breaks=6)
  })
  
}
