# Employee Data - Summary
empdata<-read.csv(file.choose(),sep=",",header = T)
summary(empdata)
names(empdata)

# Relation among predictors
pairs(~index+written+language+tech+gk,data = empdata)

#Build linear model
#empmodel<-lm(index~.,data = empdata) #empid also gets included
empmodel<-lm(index~written+language+tech+gk,data = empdata)
summary(empmodel)


#no relation
empmodel<-lm(index~written+language+tech+gk,data = empdata)
summary(empmodel)

empdata$pred<-fitted(empmodel)
head(empdata)
empdata$res<-residuals(empmodel)
head(empdata)

"plot must be random indicates no heteroscedasticity"
plot(empdata$pred,empdata$res,col="red")

"QQ Plot"#to check where the graph show normal curve or not
qqnorm(empdata$res,col="blue")  
qqline(empdata$res,col="blue")  

"Shapiro test of residual, expected p value > 0.05"
shapiro.test(empdata$res)#
#We prefer to accept H0(p value>0.05)


"to apply the model on new data..."
newempdata<-read.csv("newempdata.csv",header=T)  
newempdata$pred<-predict(empmodel,newempdata)  
newempdata


"to check the multicolinearity all VIFs should be less than 5"
library(car)#check where my indipendent variables are dependent are not if its vale greater then 5 it means it depends on other factors
vif(empmodel)
#take any two columns, add some random number and include them in data
empdata2<-read.csv("empdata.csv",sep=",",header = T)
empmodel3<-lm(index~.,data = empdata2)
summary(empmodel3)
vif(empmodel3)#Variance Inflation Factor


"detecting heteroscedasticity using ncvtest"
"p value should be greater than 0.05 to indicate homoscedasticity"
library(car)
ncvTest(empmodel3,~written+language+tech+ gk)
#homoscedasticity indicated that the residual or error is same for all value
# simple bivariate example can help to illustrate heteroscedasticity: Imagine we have data on family income and spending on luxury items.  Using bivariate regression, we use family income to predict luxury spending.  As expected, there is a strong, positive association between income and spending.  Upon examining the residuals we detect a problem - the residuals are very small for low values of family income (almost all families with low incomes don't spend much on luxury items) while there is great variation in the size of the residuals for wealthier families (some families spend a great deal on luxury items while some are more moderate in their luxury spending).  This situation represents heteroscedasticity because the size of the error varies across values of the independent variable.  Examining a scatterplot of the residuals against the predicted values of the dependent variable would show a classic cone-shaped pattern of heteroscedasticity.



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
#Outlinears check which data far from actual data

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
kfolds<-trainControl(method="cv",number=4)#cross validation
model<- train(index~written+language+tech+gk,data=empdata,method="lm",  trControl=kfolds)
model

# repeated k fold cross validation
library(caret)
kfolds<-trainControl(method="cv",number=4, repeats=5)
model<- train(index~written+language+tech+gk,data=empdata,method="lm",  trControl=kfolds)
model

#LOOCV Leave-One-Out Cross-Validation
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