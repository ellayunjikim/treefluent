#Authors: Colin Finke, Divyansh Singh, Eamon Worden, Ella Kim, Harry MacArthur
#Date: May 11, 2022
#Purpose: Creating visualizations to help understand student engagement and assess departments learning outcomes in comparison to college wide learning outcomes

library(shiny) #load shiny package
library(shinydashboard)
library(leaflet)
library(readxl)
library(shiny)
library(ggplot2)
library(datasets)
library(dplyr)
library(tidyr)
library(collapsibleTree)
library(data.tree)
library(DiagrammeR)
library(vtree)
library(stringr)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library("SnowballC")
library(plotly)
library(shinyjs)
library(shinyBS)

# IMPORT FILES first 
#LearningGoalsDataset <- read_excel("LearningGoalsDataset.xlsx")

LearningGoalsDataset <- rename(LearningGoalsDataset, 'Specific_LO' = 'Learning outcome being assessed')
LearningGoalsDataset <- rename(LearningGoalsDataset, 'CWLO' = 'College-Wide Learning Outcome Num')
LearningGoalsDataset <- filter(LearningGoalsDataset, Interpretation != "NA")

WCLGD <- read_excel("LearningGoalsDataset.xlsx")
dataset<- read_excel("CourseData.xlsx")

#preprocessing for Q2-word cloud

#a list of all the words we plan to search for to see if a course description meets a specific learning outcome
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

#generating LOs for each course
#making column
WCLGD <- 
  WCLGD %>%
  mutate(LOs = "")
#adding the number to each 'LOs' column if it matches the key words for that LO
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

#counting number of matches between professor LO and generated LO
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
#formatting
WCLGD <- rename(WCLGD, 'LO_Num'='College-Wide Learning Outcome Num')
WCLGD <- rename(WCLGD, 'Program'='Academic program')

set.seed(1234)
#end preprocessing for Q2


LearningGoalsDataset$pathString <- 
  paste("Department", LearningGoalsDataset$'Academic program', 
        LearningGoalsDataset$'Course Num',
        LearningGoalsDataset$'CWLO', 
        sep = "/")


o <- as.Node(LearningGoalsDataset)

# change to numeric 
LearningGoalsDataset$"CWLO"<- as.numeric(LearningGoalsDataset$"CWLO")

selecto <- function(x) {
  if(x == 2){
    return(dataset$Program)}
  else if(x==3){
    return (dataset$LO_Num)}
  else if(x==4){
    return (dataset$FYear)
  }else if(x==5){
    return (dataset$Percent_learned)}
}

minYear <- min(dataset$Test_Year)
maxYear <- max(dataset$Test_Year)

#HEADER
header <- dashboardHeader(title = "TREEFLUENT",
                          titleWidth = 200)

#SIDEBAR
sidebar <- dashboardSidebar(
  #--------------------BY SUBMENU ITEMS------------------#
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("list-alt")),
    menuItem("Collapsible Tree", tabName = "firstivs", icon = icon('leaf', lib = 'glyphicon')),
    menuItem("Vertical Tree", tabName = "vertical", icon = icon('circle-arrow-down', lib = 'glyphicon')),
    menuItem("Visualization 1", tabName = "vis1",icon = icon('time', lib = 'glyphicon')),
    menuItem("Visualization 2", tabName = "vis2", icon = icon('cloud', lib = 'glyphicon'))
  ))



#BODY
body <- dashboardBody(
  tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }'))),
  
  
  #---------------------DIVIDED BY TAB ITEMS------------------#
  tabItems(
    #----First Visualization----#
    
    tabItem(tabName = "firstivs",  # Application title
            titlePanel("Interactive Collapsible Tree"),
            
            # Sidebar with a select input for the root node
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  "hierarchy", "Tree hierarchy",
                  choices = c(
                    "Program Group","Academic program", "Course Num", "CWLO",
                    "Specific_LO", "Interpretation","Academic year"
                  ),
                  selected = c("Program Group","Academic program","Course Num", "CWLO", "Specific_LO"),
                  multiple = TRUE
                ),
                selectInput(
                  "fill", "Node color",
                  choices = c("CWLO", "Course Num"),
                  selected = "CWLO"
                ),
                tags$p("The node you most recently clicked:"),
                verbatimTextOutput("str"),
                tags$br(),
                p("Description: On the right is an interactive collapsible tree, which maps out the tree based on users' input format of tree hierarchy."),
                p("User can generate trees of their choice based on the ordering the variables in the box above named 'Tree Hierarchy'."),
                p("It can be an efficient tool to find paths through the specific department, narrow down to course number, and see what type of learning outcomes they have."),
                p("It can also be efficient if we order the path to start from the CWLO number and see which learning outcome is most used by various departments and more.")
              ),
              
              # Show a tree diagram with the selected root node
              mainPanel(
                collapsibleTreeOutput("plot", height = "500px")
              ))),
    
    #----Question 1----#
    tabItem(tabName = "vis1",
            fluidRow(
              box(width = 500,
                  titlePanel("Learning Outcomes Over Time"),
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                    sidebarPanel(
                      #filtering by learning outcome
                      checkboxGroupInput("LO",
                                         label = h4("Learning Outcome"),
                                         choices = list("1",
                                                        "2",
                                                        "3",
                                                        "4",
                                                        "5",
                                                        "6"),
                                         selected = c(1, 2, 3, 4, 5, 6)),
                      #filtering by program
                      selectInput("Program",
                                  label = h4("Programs"),
                                  choices = unique(dataset[["Program"]]),
                                  multiple = TRUE,
                                  selected = unique(dataset[["Program"]]),
                      ),
                      #filtering by range of years
                      sliderInput("years",
                                  label = h4("Time Range"),
                                  min = minYear,
                                  max = maxYear,
                                  sep = "",
                                  value = c(minYear, maxYear)
                      ),
                      #coloring for graph
                      selectInput("Color",
                                  label = h4("Color Scale"),
                                  choices = list("YlGnBu",
                                                 "YlOrRd",
                                                 "RdYlGn",
                                                 "Spectral"),
                                  multiple = FALSE,
                                  selected = "RdYlGn"
                      ),
                      width = 3,
                    ),
                    
                    
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      plotlyOutput("plotman")
                    )
                  )
              ))),
    #Q2
    tabItem(tabName = "vis2",
            # Application title
            titlePanel("Learning Outcomes Different Departments are Engaged in"),
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
              sidebarPanel(
                selectInput("programs",
                            label = h5("Programs"),
                            choices = unique(WCLGD[["Program"]]), #letting users pick which programs they want to visualize
                            multiple = TRUE,
                            selected = unique(WCLGD[["Program"]]),
                ),
                p("Description: up top is a word cloud, which displays how prevelant each learning goal was across every selected department."),
                p("To generate the word cloud we first run a text analysis program to decide which learning outcome each class really was (and occasionally they were multiple learning outcomes)."),
                p("We then generate a word cloud using that data to help visualize the prevelance of each learning outcome in the selected departments."),
                p("Undernearth that is how often our text analysis program agrees with the professors selected learning outcome. Once we've generated a list of learning outcomes, we check to see if 
        the professor's selected learning outcome matches one of the potential learning outcomes we've generated, then we add error bars to visualize roughly how often our program and the professors agree.")
              ),
              
              # Outputting wordcloud and barchart
              mainPanel(
                plotOutput("wordcloud"),
                plotOutput("barchart"),
              ))),
    
    tabItem(tabName = "vertical",
            fluidRow(
              box(titlePanel("Vertical Tree"), width = 12,solidHeader = TRUE, (div(style='width:1400px;overflow-x: scroll;height:800px;overflow-y: scroll;',
                                                                                   mainPanel(vtreeOutput(outputId = "vTree1", width="1000%", height="800px"))))))),
    
    tabItem(tabName = "home",
            HTML(
              paste( h2("COLLEGE-WIDE LEARNING OUTCOMES:", style = "font-size:50px;"),
                     HTML(
                       paste(
                         h4(strong(tags$a(href="https://www.grinnell.edu/academics/centers-programs/ctla/college-wide", "CLICK HERE for more on Grinnell College Wide Learning Outcomes (CWLO)"))),
                         h3("The Grinnell College Faculty has endorsed the following six learning outcomes as a basis for our learning assessment activities:", style = "font-size:27px;"),
                         h2(strong("Outcome #1")),
                         h3("Students develop creative and critical thinking skills that allow them to analyze the work of others, formulate relevant questions, and respond to those questions in a substantive way using quantitative and qualitative evidence."),
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-1?v2node", "Outcome 1 Details"))),
                         h2(strong("Outcome #2")),
                         h3("Students develop a sense of social responsibility and fairness that guides them in their personal and professional lives."),   
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-2?v2node", "Outcome 2 Details"))),
                         h2(strong("Outcome #3")),
                         h3(" Students develop the ability to communicate clearly and persuasively in various modes for various purposes and audiences."),
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-3", "Outcome 3 Details"))),
                         h2(strong("Outcome #4")),
                         h3("Students develop the ability to continue learning independently and collaboratively."),
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-4", "Outcome 4 Details"))),
                         h2(strong("Outcome #5")),
                         h3("Students develop the ability to approach a question from multiple perspectives, representing a diversity of ideas and experiences."),
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-5", "Outcome 5 Details"))),
                         h2(strong("Outcome #6")),
                         h3("Students pursue a chosen field of study in depth and develop understanding of a core body of knowledge in that field as well as the ability to employ modes of inquiry appropriate to that field."),
                         h4(strong(tags$a(href = "https://www.grinnell.edu/academics/centers-programs/ctla/college-wide/outcome-6?v2node", "Outcome 6 Details"))))))))
  ))

#Define UI for the shiny application
shinyUI(fluidPage(
  
  dashboardPage(skin = "black",
                #HEADER
                header,
                #SIDEBAR,
                sidebar,
                #BODY
                body
  )
  
  
))
