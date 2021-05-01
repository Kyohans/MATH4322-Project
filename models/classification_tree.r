#import dataset stroke-data.csv
stroke.data <- read.csv("../../stroke-data.csv", na.strings="N/A", stringsAsFactors = TRUE)
stroke.data = na.omit(stroke.data); stroke.data = stroke.data[-1]

# Downsample stroke column to fix imbalance
#install.packages("caret")
library(caret)
stroke.data$stroke = as.factor(stroke.data$stroke)
stroke.data2 = downSample(stroke.data[,-c(11)], stroke.data$stroke, list = FALSE, yname = "stroke")

# Splitting observations into training and testing sets (80-20 split)
set.seed(3)
train = sample(nrow(stroke.data2), nrow(stroke.data2)*.80)
train.stroke = stroke.data2[train,]
test.stroke = stroke.data2[-train,]

# Classification tree
library(tree)
tree.stroke = tree(stroke~., data = stroke.data2, subset = train)
plot(tree.stroke); text(tree.stroke, pretty = 1)

# Cross-validation
set.seed(3)
cv.stroke = cv.tree(tree.stroke, FUN = prune.misclass)
cv.stroke
plot(cv.stroke$size,cv.stroke$dev,type="b")

#so lowest CV is 92 with no of nodes = 5

#now we prune the tree to 5
prune.stroke = prune.misclass(tree.stroke,best = 5)
summary(prune.stroke)
plot(prune.stroke); text(prune.stroke, pretty = 0)
tree.pred = predict(prune.stroke,test.stroke,type = "class")

# what should be the second parameter for table. I tried " test.stroke "
# but all arguments must have the same length.
table(tree.pred)
