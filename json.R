
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
  )
print(BMI)


# Load the package required to read JSON files.
library("rjson")

result <- fromJSON(file = "marks.json")
#result <- fromJSON(file = file.choose())

# Print the result.
print(result)

jsondata <- as.data.frame(result)
print(jsondata)

#structure of data frame
str(jsondata)

#statistical summary and nature of data
summary(jsondata)

#extract few columns
result <- data.frame(jsondata$ATTD ,jsondata$DEPT )
print(result)


# row selection
jsondata[1:5,]
jsondata[6:10,]
jsondata[c(1,3,5),]
jsondata[c(1,3,5,6:9),]
jsondata[c(-1,-3,-5),]

# column selection
jsondata[1:5,1:2]
jsondata[6:10,3:4]
jsondata[c(1,3,5),c(1,3)]
jsondata[c(1,3,5,6:9),c(1,3,4)]
jsondata[c(-1,-3,-5),c(-2)]


#subset selection

res <-subset(jsondata, DEPT=="CS")
print(res)

tempdata <- subset(jsondata, DEPT=="CS" & PERCENT>50)

subset(tempdata, select = c(-DEPT))
subset(tempdata, select = c(PERCENT,ROLL))

tempdata <- subset(jsondata, !(DEPT=="CS") & PERCENT>50)
print(tempdata)

mean(jsondata$ATTD)
mean(jsondata$PERCENT)

aggregate(PERCENT~DEPT, data=jsondata, FUN=mean)

aggregate(PERCENT~DEPT+SEM, data=jsondata, FUN=mean)


aggregate(cbind(PERCENT,ATTD)~DEPT+SEM, data=jsondata, FUN=mean)

  fr <-table(jsondata$DEPT, jsondata$SEM)
  
  prop.table(fr)
  
  boxplot(jsondata$ATTD)
  boxplot(jsondata$PERCENT)
  
  boxplot(ATTD~DEPT, data=jsondata)
  
  boxplot(ATTD~DEPT, data=jsondata, col=c("red","green", "blue"))
  
  
  hist(jsondata$ATTD)
  
  sampledata <- 	data.frame(
    gender = c("Male", "Male","Female", "Female", "Male", "Male"), 
    loc = c("Mumbai", "Delhi","Pune", "Pune", "Delhi", "Pune"), 
    empname = c("a","b","c","d","e","f")
   
  )
  
  sampledata
  
  sampledata[order(sampledata$loc),]
  
  library(dplyr)
  jsondata[ order(jsondata$DEPT, desc(jsondata$SEM)), ]

#to check whether any columns have NA values
result2 <- fromJSON(file = "marks.json")
  result2
  mydata <- as.data.frame(result2)
  mydata[2,2] <- NA
  mydata[6,3] <- NA
  colSums(is.na(mydata))
  colSums(is.na(mydata))
  nmiss<-sum(is.na(mydata$ATTD))
  nmiss
