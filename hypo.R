
To check whether the data follows normal distribution 
"test for normal destribution"
data1<-read.csv("…….csv",sep=",",header = T)
shapiro.test(data1$colname)
# p value should be greater than 0.05

#"one sample t test"
#One sample t test is used to test the hypothesis about a single  population mean.
apple<-read.csv(file.choose(),sep=",",header = T)
summary(apple)
t.test(apple$C1, alternative="greater", mu=97)

#"independent t test "
#The independent-samples t-test compares the means of two  independent groups
#h0 = same, h1 = not same
time<-read.csv(file.choose(),sep=",",header = T)
summary(time)
t.test(videodata$TotalCOld,videodata$TotalDDEMO, alternative="two.sided",  var.equal=TRUE)


#"paired t test " Dependent samples
time1<-read.csv(file.choose(),sep=",",header = T)
t.test(time1$time_before,time1$time_after,alternative = "greater",paired = T)

"t test for correlation " # h0 - no correlation, h1 - there is correlation
cor<-read.csv(file.choose(),sep=",",header = T)
summary(cor)
cor.test(cor$aptitude,cor$job_prof,alternative = "two.sided",method="pearson")

"t test for variance "
var<-read.csv(file.choose(),sep=",",header = T)
summary(var)
var.test(var$time_g1,var$time_g2,alternative = "two.sided")

apple<-read.csv(file.choose(),sep=",",header = T)
summary(apple)
t.test(apple$C1, alternative="greater", mu=100)
