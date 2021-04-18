#import dataset stroke-data.csv
stroke.data <- read.csv("~/stroke-data.csv", na.strings="N/A", stringsAsFactors = TRUE)
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


# RANDOM FOREST MODEL


library(tidyverse)
base_stroke<- read.csv("~/stroke-data.csv")
head(base_stroke)
#     id gender age hypertension heart_disease ever_married     work_type Residence_type #avg_glucose_level  bmi  smoking_status stroke
#1  9046   Male  67            0             1          Yes       Private          Urban            228.69 36.6 #formerly smoked      1
#2 51676 Female  61            0             0          Yes Self-employed          Rural            202.21  N/A    #never smoked      1
#3 31112   Male  80            0             1          Yes       Private          Rural            105.92 32.5    #never smoked      1
#4 60182 Female  49            0             0          Yes       Private          Urban            171.23 34.4          #smokes      1
#5  1665 Female  79            1             0          Yes Self-employed          Rural            174.12   24    #never smoked      1
#6 56669   Male  81            0             0          Yes       Private          Urban            186.21   29 #formerly smoked      1
base_stroke$Indice<-c(1:nrow(base_stroke))
base_stroke<-base_stroke[,c(13,1:12)]
base_stroke$ever_married<-as_factor(base_stroke$ever_married)
base_stroke$work_type<-as_factor(base_stroke$work_type)
base_stroke$Residence_type<-as_factor(base_stroke$Residence_type)
base_stroke$smoking_status<-as_factor(base_stroke$smoking_status)

base_stroke$stroke<-as_factor(base_stroke$stroke)

base_stroke$bmi<-as.numeric(base_stroke$bmi)
#Warning message:
#NAs introduced by coercion 
head(base_stroke,4)
#  Indice    id gender age hypertension heart_disease ever_married     work_type #Residence_type avg_glucose_level  bmi  smoking_status stroke
#1      1  9046   Male  67            0             1          Yes       Private          Urban            228.69 36.6 #formerly smoked      1
#2      2 51676 Female  61            0             0          Yes Self-employed          Rural            202.21   #NA    never smoked      1
#3      3 31112   Male  80            0             1          Yes       Private          Rural            105.92 32.5    #never smoked      1
#4      4 60182 Female  49            0             0          Yes       Private          Urban            171.23 #34.4          smokes      1
qtd_NAs<-c()

for(i in 1 : length(base_stroke)){
    
    qtd_NAs[i]<-sum(is.na(base_stroke[,i]))
}
qtd_NAs
# [1]   0   0   0   0   0   0   0   0   0   0 201   0   0
media_bmi<-base_stroke %>% group_by(gender) %>% summarize(media_Bmi=mean(bmi,na.rm = TRUE),n=n())

for(i in 1: nrow(base_stroke)){
    if(is.na(base_stroke$bmi[i])){
        if(base_stroke$gender[i] == "Male"){
            base_stroke$bmi[i]<-media_bmi$media_Bmi[1]
        }
        if(base_stroke$gender[i] == "Female"){
            base_stroke$bmi[i]<-media_bmi$media_Bmi[2]
        }
        if(base_stroke$gender[i] == "Other"){
            base_stroke$bmi[i]<-media_bmi$media_Bmi[3]
        }
    }
}
base_stroke<-base_stroke[,-c(1:2)]
library(caTools)
set.seed(10)
div<-sample.split(Y = base_stroke$stroke,SplitRatio = 0.70)
base_training<-subset(base_stroke,subset = div == TRUE)
base_test<-subset(base_stroke,subset = div == FALSE)
library(randomForest)
set.seed(1)
model_RF<-randomForest(formula = stroke ~.,data = base_training,ntree =20)
model_RF

#Call:
# randomForest(formula = stroke ~ ., data = base_training, ntree = 20) 
#               Type of random forest: classification
#                     Number of trees: 20
#No. of variables tried at each split: 3
#
#        OOB estimate of  error rate: 5.68%
#Confusion matrix:
#     0  1 class.error
#0 3370 31 0.009114966
#1  172  2 0.988505747

prediction<-predict(model_RF,newdata = base_test[,-11])

confusionMatrix<-table(base_test$stroke,prediction)
confusionMatrix
#   prediction
#       0    1
#  0 1457    1
#  1   75    0
accuracy<-(confusionMatrix[1] + confusionMatrix[4])/ sum(confusionMatrix)
accuracy
#[1] 0.950424
compare<-data.frame(Real=base_test$stroke,Prediction=prediction)
head(compare)
#   Real Prediction
#20    1        0
#21    1        0
#23    1        0
#26    1        0
#27    1        0
#29    1        0

