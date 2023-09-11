install.packages("data.tree")
library(data.tree)

out <- Node$new("Grinnell College")

X <- out$AddChild("X")
X1 <- X$AddChild("1")
X6 <- X$AddChild("6")

S <- out$AddChild("S")
S3 <- S$AddChild("3")
S4 <- S$AddChild("4")
S5 <- S$AddChild("5")


P <- out$AddChild("P")
P5 <- P$AddChild("5")


M <- out$AddChild("M")
M4 <- M$AddChild("4")

print(out)

install.packages("DiagrammeR")
library(DiagrammeR)
plot(out)


test$pathString <- 
  paste("Department", test$program, test$CourseNum,test$outcome, sep = "/")

outcome <- as.Node(test)
print(outcome, "outcome", limit = 26)
plot(outcome)

# the interactivity section
install.packages("collapsibleTree")
library(collapsibleTree)

# error message: Error in if (length(nms) != n || any(nms == "")) stop("'options' must be a fully named list, or have no names (NULL)") : missing value where TRUE/FALSE needed
collapsibleTree(
  test,
  hierarchy = c("program", "CourseNum", "outcome","pathString"),
  width = 800
)
