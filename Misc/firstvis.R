library(data.tree)
library(DiagrammeR)
library(collapsibleTree)

summary(LearningGoalsDataset)
View(LearningGoalsDataset)

LearningGoalsDataset$pathString <- 
  paste("Department", LearningGoalsDataset$'Academic program', 
        LearningGoalsDataset$'Course Num',
        LearningGoalsDataset$'College-Wide Learning Outcome Num', 
        sep = "/")

o <- as.Node(LearningGoalsDataset)
print(o, "outcome", limit = 26)

#-----------#

# change to numeric 
LearningGoalsDataset$`College-Wide Learning Outcome Num`<- as.numeric(LearningGoalsDataset$`College-Wide Learning Outcome Num`)

LearningGoalsDataset$tooltip <- paste0(
  LearningGoalsDataset$"College-Wide Learning Outcome Num",
  "<br>Goal: ",
  LearningGoalsDataset$"Learning outcome being assessed"  
  )

firstvis <- collapsibleTree(
  LearningGoalsDataset,
  hierarchy = c("Program Group", "Academic program", "Course Num","College-Wide Learning Outcome Num","Learning outcome being assessed"),
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
    rep("blue", length(unique(paste(LearningGoalsDataset$"College-Wide Learning Outcome Num", LearningGoalsDataset$"Course Num")))),
    rep("black", length(unique(paste(LearningGoalsDataset$"Learning outcome being assessed", LearningGoalsDataset$"Course Num")))),
    "black"
  ),
  tooltipHtml = "tooltip",
  width = 4000,
  height = 400,
  zoomable = FALSE,
  fillByLevel = TRUE
)

# prints the first visualization
print(firstvis)


