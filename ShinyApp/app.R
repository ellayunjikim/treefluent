#Authors: Colin Finke, Divyansh Singh, Eamon Worden, Ella Kim, Harry MacArthur
#Date: May 11, 2022
#Purpose: Creating visualizations to help understand student engagement and assess departments learning outcomes in comparison to college wide learning outcomes

library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(ggiraph)

source("ui.R")
source("server.R")

shinyApp(ui, server)
