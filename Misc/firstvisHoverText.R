library(data.tree)
library(DiagrammeR)
library(collapsibleTree)



summary(LearningGoalsDataset)



LearningGoalsDataset$pathString <-
  paste("Department", LearningGoalsDataset$'Academic program',
        LearningGoalsDataset$'Course Num',
        LearningGoalsDataset$'College-Wide Learning Outcome Num',
        sep = "/")



o <- as.Node(LearningGoalsDataset)
print(o, "outcome", limit = 26)
plot(o)



#-----------#



# change to numeric
LearningGoalsDataset$`College-Wide Learning Outcome Num`<- as.numeric(LearningGoalsDataset$`College-Wide Learning Outcome Num`)



firstvis <- collapsibleTree(
  LearningGoalsDataset,
  hierarchy = c("Academic program", "Course Num","College-Wide Learning Outcome Num"
                ,"Learning outcome being assessed"),
  width = 4000,
  height = 800,
  zoomable = FALSE,
  tooltip = TRUE,
  fillByLevel = TRUE
)



# prints the first visualization
print(firstvis)