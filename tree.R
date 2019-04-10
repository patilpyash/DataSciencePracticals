install.packages("partykit")
install.packages("CHAID",repos = "http://R-Forge.R-project.org",type="source")
library(CHAID)
library(partykit)

mydata<-data.frame(iris)
model2<-ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data=mydata)
plot(model2)
str(iris)
model3 <-predict(model2,iris) # Prediction

model2<-chaid(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data=mydata)
mydata$Sepal.Length=factor(mydata$Sepal.Length)
mydata$Sepal.Width=factor(mydata$Sepal.Width)
mydata$Petal.Length=factor(mydata$Petal.Length)
mydata$Petal.Width=factor(mydata$Petal.Width)
plot(model2)

mtcars
mydata<-data.frame(mtcars)
amodel<-ctree(mpg ~ cyl+disp+hp+wt+gear,data=mydata)
plot(amodel)

mydata$cyl=factor(mydata$cyl)
mydata$disp=factor(mydata$disp)
mydata$hp=factor(mydata$hp)
mydata$wt=factor(mydata$wt)
mydata$gear=factor(mydata$gear)
mydata$mpg = factor(mydata$mpg)
bmodel<-chaid(mpg ~ cyl+disp+hp+wt+gear,data=mydata)
plot(bmodel)


#Classfication Tree to predict class of flower
library("rpart")
png(file="tree5.png")
mydata<-data.frame(iris)
amodel<-rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data=mydata, method="class")
plot(amodel)
text(amodel)
dev.off()

#Classfication Tree to predict defaulter status
png(file="tree6.png")
mydata<-read.csv("bankloan.csv", header = T, sep = ",")
head(mydata)
amodel<-rpart(DEFAULTER~AGE+EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT,data=mydata, method="class")
plot(amodel)
text(amodel)
dev.off()


#Regression Tree to predict mileage
library("rpart")
png(file="tree3.png")
mydata<-data.frame(mtcars)
amodel<-rpart(mpg ~ cyl+disp+hp+wt+gear,data=mydata, method="anova" )
plot(amodel)
text(amodel)
dev.off()

#regression tree to predict index
library("rpart")
mydata<-read.csv("empdata.csv", header = T, sep = ",")
head(mydata)
amodel<-rpart(index ~ tech+written+language+gk ,data=mydata, method="anova" )
png(file="tree4.png")
plot(amodel)
text(amodel)
dev.off()
