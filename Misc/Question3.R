#libraries
library(readxl)
library(shiny)
library(ggplot2)

#Importing datasets
dataset <- read_excel("Downloads/Dashboard-data-share.xlsx")
View(dataset)  


ggplot(data = dataset) +
  geom_point(mapping = aes_string(x = "Program", y = "Percent_learned", shape = "FYear", color = "LO_Num"))

