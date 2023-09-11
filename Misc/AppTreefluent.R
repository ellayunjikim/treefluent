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
library(shiny)
library(ggplot2)

#dataset <- read_excel("/Users/harrymacarthur/Desktop/CSC 324/Group Proj/Dashboard-data-share_time.xlsx")
#Harry use the below command when you run this to set your working directory so you dont need the whole path
#setwd('/Users/harrymacarthur/Desktop/CSC 324/Group Proj/')
dataset <- read_excel("Dashboard-data-share_time.xlsx")
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("the thing that does"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("select", 
                      label = h2("X-Axis Variable"),
                      choices = c("Program",
                                  "LO_Num", 
                                  "FYear",
                                  "Percent_learned",
                                  "Test_Year"),
                      selected = "Program"
          ),
          
          selectInput("select2", 
                      label = h2("Y-Axis Variable"),
                      choices = c("Program",
                                  "LO_Num", 
                                  "FYear",
                                  "Percent_learned",
                                  "Test_Year"),
                      selected = "Percent_learned"
          ),
          
          selectInput("col1", 
                      label = h5("High Color"),
                      choices = c("red",
                                  "green", 
                                  "gold",
                                  "blue",
                                  "pink",
                                  "dark green",
                                  "deepskyblue"),
                      selected = "dark green"
          ),
          
          selectInput("col2", 
                      label = h5("Mid Color"),
                      choices = c("red",
                                  "green", 
                                  "gold",
                                  "blue",
                                  "pink",
                                  "dark green",
                                  "deepskyblue"),
                      selected = "green"
          ),
          
          selectInput("col3", 
                      label = h5("Low Color"),
                      choices = c("red",
                                  "green", 
                                  "gold",
                                  "blue",
                                  "pink",
                                  "dark green",
                                  "deepskyblue"),
                      selected = "red"
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          
           plotOutput(outputId = "distPlot"),
           p("descriptive text goes here"),
           p("more descriptive text goes here"),
           p("even more descriptive text goes here")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
