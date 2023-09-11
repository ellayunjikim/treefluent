#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readxl)
library(stringr)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library("SnowballC")

#setwd('C:\\Users\\16032\\OneDrive - Grinnell College\\Desktop\\Year 3 Sem 2\\CSC324\\Treefluent')

WCLGD <- read_excel("LearningGoalsDataset.xlsx")
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


WCLGD <- rename(WCLGD, 'LOBA' = 'Learning outcome being assessed')

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

#LearningGoalsDataset <- rename(LearningGoalsDataset, 'Learning outcome being assessed'='LOBA')



WCLGD <- rename(WCLGD, 'LO_Num'='College-Wide Learning Outcome Num')
WCLGD <- rename(WCLGD, 'Program'='Academic program')

set.seed(1234)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Learning Outcomes Different Departments are Engaged in"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("programs",
                  label = h5("Programs"),
                  choices = unique(WCLGD[["Program"]]),
                  multiple = TRUE,
                  selected = unique(WCLGD[["Program"]]),
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("wordcloud"),
      plotOutput("barchart"),
      p("Description: up top is a word cloud, which displays how prevelant each learning goal was across every selected department."),
      p("To generate the word cloud we first run a text analysis program to decide which learning outcome each class really was (and occasionally they were multiple learning outcomes)."),
      p("We then generate a word cloud using that data to help visualize the prevelance of each learning outcome in the selected departments."),
      p("Undernearth that is how often our text analysis program agrees with the professors selected learning outcome. Once we've generated a list of learning outcomes, we check to see if 
        the professor's selected learning outcome is one of the potential learning outcoems we've generated, then we add error bars to visualize roughly how often our program and the professors agree.")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #data <- filter(WCLGD, LO_Num %in% input$LO, Program %in% input$programs)
  
  output$wordcloud <- renderPlot({
    data <- filter(WCLGD, Program %in% input$programs)
    #data <- LearningGoalsDataset
    #need to filter using program
    
    wordnumbs <- ""
    
    for (obs in data$'LOs'){
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
    
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,
              scale=c(6,10), random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$barchart <- renderPlot({
    #work for bar chart
    data <- filter(WCLGD, Program %in% input$programs)
    LO=c(1,2,3,4,5,6)
    correct=c(0,0,0,0,0,0)
    incorrect=c(0,0,0,0,0,0)
    
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
                    width=0.4, colour="orange", alpha=0.8, size=1.3) +
      scale_x_continuous(n.breaks=6)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
