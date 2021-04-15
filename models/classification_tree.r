stroke.data <- read.csv("../stroke-data.csv", na.strings="N/A", stringsAsFactors = TRUE)
stroke.data = na.omit(stroke.data); stroke.data = stroke.data[-1]

# Downsample stroke column to fix imbalance
#install.packages("caret")
library(caret)
stroke.data$stroke = as.factor(stroke.data$stroke)
stroke.data2 = downSample(stroke.data[,-c(11)], stroke.data$stroke, list = FALSE, yname = "stroke")

# Splitting observations into training and testing sets (80-20 split)
set.seed(5)
train = sample(nrow(stroke.data2), nrow(stroke.data2)*.80)
train.stroke = stroke.data2[train,]
test.stroke = stroke.data2[-train,]

# Classification tree
library(tree)
tree.stroke = tree(stroke~., data = stroke.data2, subset = train)
plot(tree.stroke); text(tree.stroke, pretty = 1)
