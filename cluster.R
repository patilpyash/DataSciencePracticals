bankloan<-read.csv("bankloan.csv",header=T)  
bankloanca<-subset(bankloan,select=c(-AGE,-SN))  
CL<-kmeans(bankloanca,centers = 3, nstart = 3)
CL

CL$centers
head(CL$cluster)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

fviz_cluster(CL, data = bankloan)

##

library(NbClust)
nb <- NbClust(bankloan, diss=NULL, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans",index = "all", alphaBeale = 0.1)
#nb <- NbClust(bankloan,  min.nc=2, max.nc=5, method = "kmeans",index = "all")
nb$All.index
nb$All.CriticalValues
nb$Best.nc


CL<-kmeans(bankloanca,centers = 3, nstart = 1)
CL
fviz_cluster(CL, data = bankloan)
