# Employee Data - Summary
empdata<-read.csv("empdata.csv",sep=",",header = T)
summary(empdata)
names(empdata)

# Relation among predictors
pairs(~index+written+language+tech+gk,data = empdata)

#Build linear model
#empmodel<-lm(index~.,data = empdata) #empid also gets included
empmodel<-lm(index~written+language+tech+gk,data = empdata)

#global testing to check whether 
#at least one variable has significant impact
#Check p value, it should be less than 0.05
#R squared values should be closer to 100%
#significant variables are marked with *
summary(empmodel)
plot(empmodel)


#no relation
empmodeltemp<-lm(empid~written+language+tech+gk,data = empdata)
summary(empmodeltemp)

#to get the values of response variable
empdata$pred<-fitted(empmodel)
head(empdata)

#to get the values of resuduals
empdata$res<-residuals(empmodel)
head(empdata)

"to apply the model on new data..."
newempdata<-read.csv("newempdata.csv",header=T)  
newempdata$pred<-predict(empmodel,newempdata)  
newempdata


"Shapiro test of residual, expected p value > 0.05"
shapiro.test(empdata$res)


"to check the multicolinearity all VIFs should be less than 5"
library(car)
vif(empmodel)


#take any two columns, add some random number and include them in data
empdata2<-read.csv("testempdata.csv",sep=",",header = T)
empmodel3<-lm(index~.,data = empdata2)
summary(empmodel3)
vif(empmodel3)


"plot must be random indicates no heteroscedasticity"
plot(empdata$pred,empdata$res,col="red")

"QQ Plot"
qqnorm(empdata$res,col="blue")  
qqline(empdata$res,col="blue")  

"detecting heteroscedasticity using ncvtest"
"p value should be greater than 0.05 to indicate homoscedasticity"
library(car)
ncvTest(empmodel3,~written+language+tech+ gk)


#finding influential observations
library(car)
influ<-influence.measures(empmodel)
influ
#influencePlot(empmodel,id.method="identify",main="Influence Plot",sub="Circle size is proportial to Cook's Distance")
influencePlot(empmodel,  main="Influence Plot",sub="Circle size is proportial to Cook's Distance")
#remove influential observation
empdatatemp <-empdata[-(33),]
empmodeltemp<-lm(index~written+language+tech+gk,data = empdatatemp)
influ<-influence.measures(empmodeltemp)
influ
influencePlot(empmodeltemp,  main="Influence Plot",sub="Circle size is proportial to Cook's Distance")

#hold out validation in R
empdata
library(caret)
emphold<-createDataPartition(empdata$index,p=0.8,list=FALSE)
head(emphold)
dim(emphold)
traindata <- empdata[emphold,]
testdata <-empdata[-emphold,]
traindata
testdata

empmodel<-lm(index~written+language+tech+gk,data = traindata)
traindata$res<-residuals(empmodel)
head(traindata)
RMSEtrain<-sqrt(mean(traindata$res**2))
RMSEtrain 

testdata$pred<-predict(empmodel,testdata)  
testdata$res<-(testdata$index-testdata$pred)  
RMSEtest<-sqrt(mean(testdata$res**2))
RMSEtest
#there should not be much difference in RMSEtrain & RMSEtest

# k fold cross validation
library(caret)
kfolds<-trainControl(method="cv",number=4)
model<- train(index~written+language+tech+gk,data=empdata,method="lm",  trControl=kfolds)
model

# repeated k fold cross validation
library(caret)
kfolds<-trainControl(method="cv",number=4, repeats=5)
model<- train(index~written+language+tech+gk,data=empdata,method="lm",  trControl=kfolds)
model

#LOOCV
kfolds<-trainControl(method="LOOCV")
model<- train(index~written+language+tech+gk,data=empdata,method="lm",  trControl=kfolds)
model


# stepwise regression
null<-lm(index~1,data=empdata)
summary(null)
full<-lm(index~written+language+tech+gk,data=empdata)
summary(full)
step(null,scope=list(lower=null,upper=full),direction="forward")

step(full ,scope=list(lower=null,upper=full),direction="backward")

step(full ,scope=list(lower=null,upper=full),direction="both")


# logistic regression, bankloan file contains 600 records
bankloan<-read.csv("bankloan.csv",header=T) 
head(bankloan)

str(bankloan)
# Example of changing data formats
# age is an integer, but it should be considerred as factor/category
# we need to do the conversion 
bankloan$AGE<-factor(bankloan$AGE)  
str(bankloan)
