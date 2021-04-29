
library(tidyverse)
base_stroke<- read.csv("~/stroke-data.csv")
head(base_stroke)

base_stroke$Indice<-c(1:nrow(base_stroke))
base_stroke<-base_stroke[,c(13,1:12)]
base_stroke$ever_married<-as_factor(base_stroke$ever_married)
base_stroke$work_type<-as_factor(base_stroke$work_type)
base_stroke$Residence_type<-as_factor(base_stroke$Residence_type)
base_stroke$smoking_status<-as_factor(base_stroke$smoking_status)

base_stroke$stroke<-as_factor(base_stroke$stroke)

base_stroke$bmi<-as.numeric(base_stroke$bmi)

head(base_stroke,4)

qtd_NAs<-c()

for(i in 1 : length(base_stroke)){
  
  qtd_NAs[i]<-sum(is.na(base_stroke[,i]))
}
qtd_NAs

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

levels(base_stroke$stroke)<-c("No stroke","Had stroke")

base_stroke<-base_stroke[,-c(1:2)]
library(caTools)
set.seed(10)
div<-sample.split(Y = base_stroke$stroke,SplitRatio = 0.70)
base_training<-subset(base_stroke,subset = div == TRUE)
base_test<-subset(base_stroke,subset = div == FALSE)
library(randomForest)
set.seed(1)
model_RF<-randomForest(formula = stroke ~.,data = base_training,ntree =500)
model_RF


prediction<-predict(model_RF,newdata = base_test[,-11])

confusionMatrix<-table(base_test$stroke,prediction)
confusionMatrix

accuracy<-(confusionMatrix[1] + confusionMatrix[4])/ sum(confusionMatrix)
accuracy

compare<-data.frame(Real=base_test$stroke,Prediction=prediction)
head(compare)



