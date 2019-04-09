data("iris")
head(iris)
data(iris)
summary(iris)
library()
"to find principal component"
mypr<-prcomp(iris[,-5],scale=T)
"to understand use of scale"
plot(iris$Sepal.Length,iris$Sepal.Width)
plot(scale(iris$Sepal.Length),scale(iris$Sepal.Width))
mypr
summary(mypr)
plot(mypr)
biplot(mypr,scale=0)
iris$pred<-predict(mypr,iris,ncomp = 2)


"extract pc scores"
str(mypr)
mypr$x
iris2<-cbind(iris,mypr$x[,1:2])
head(iris2)

cor(iris[,-5],iris2[,6:7])

install.packages("pls")
library(pls)
names(iris)
pcmodel<-pcr(Sepal.Length~Species+Sepal.Width+Petal.Length+Petal.Width,ncomp=3,data=iris,scale=T)
summary(pcmodel)
iris$pred<-predict(pcmodel,iris,ncomp = 2)
head(iris)

#==

dim(mtcars)
apply(mtcars,2, mean) #2 means apply on columns ->2 for column and 1 for row
apply(mtcars, 2, var)
carmodel <- prcomp(mtcars, scale=T)
carmodel
summary(carmodel)
names(carmodel)
plot(carmodel,type="l")
biplot(carmodel,scale = 0, cex=0.65)

# music choice

newgendata <- read.csv("studdata2.csv", header = T, sep = ",")
newgenmodel <- prcomp(newgendata, scale=T)
summary(newgenmodel)
biplot(newgenmodel,scale = 0, cex=0.65)
plot(newgenmodel,type="l")

pred <- predict(mypr, newdata=iris.valid[,1:4])
