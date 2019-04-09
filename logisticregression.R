install.packages("caTools")
data<-read.csv(file.choose(),head = TRUE,sep=",")
data
library(caTools)
split<-sample.split(data, SplitRatio= 0.8)
traning<-subset(data,split==TRUE)
testing<-subset(data,split==FALSE)
model=glm(type~.,traning,family = "binomial")#. means all
summary(model)# Estimate:-coefficient
#by removing age
model=glm(type~.-age,traning,family = "binomial")#. means all
summary(model)# Estimate:-coefficient
#AIC Value is increase hence we include this age
pre<-predict(model,testing,type="response")
testing
table(ActualValue=testing$type,PredictedValue=pre>0.5)
library(ROCR)
rpred<-prediction(pre,testing$type)
rpref<-performance(rpred,"tpr","fpr")
plot(rpref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
#using graph to see threshold
table(ActualValue=testing$type,PredictedValue=pre>0.3)