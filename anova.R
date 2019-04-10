
ftest<-read.csv(file.choose(),sep=",",header=T)
var.test(ftest$time_g1,ftest$time_g2,alternative = "two.sided")


data1<-read.csv(file.choose(),sep = ",",header = T)
names(data1)
summary(data1)
head(data1)
anv<-aov(formula = satindex~dept,data=data1)
summary(anv)


data2<-read.csv(file.choose(),sep=",",header = T)
names(data2)
summary(data2)
anv1<-aov(formula = satindex~ dept+exp+dept*exp,data = data2)
summary(anv1)
