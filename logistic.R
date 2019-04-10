#glm - generalized linear model
riskmodel<- glm(DEFAULTER~AGE+EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT,family=binomial,data=bankloan)
summary(riskmodel)
model.matrix(riskmodel)

null<-glm(DEFAULTER ~ 1, family=binomial,data=bankloan)  
anova(null,riskmodel, test="Chisq")
#here h0 is rejected, means at least one variable is significant

bankloan$predprob<-round(fitted(riskmodel),2)  
head(bankloan)

library(gmodels)  
CrossTable(bankloan$DEFAULTER,fitted(riskmodel)>0.5)
head(bankloan)

table (bankloan$DEFAULTER,fitted(riskmodel)>0.5)

library(ROCR)
bankloan$predprob<-fitted(riskmodel)
pred<-prediction(bankloan$predprob,bankloan$DEFAULTER)
perf<-performance(pred,"tpr","fpr")
plot(perf)  
abline(0,1)
auc<-performance(pred,"auc")  
auc@y.values
# Perfect model if C=1  Worthless	if C=0.5

riskmodel<- glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,family=binomial,data=bankloan)
library(ResourceSelection)
#function hoslem.test requires observed Y and predicted probs  
hltest<-hoslem.test(bankloan$DEFAULTER,fitted(riskmodel),g=10)
hltest

confusionMatrix(traindata$predY,traindata$DEFAULTER,positive="1")

bankloan
heads(bankloan)
riskmodel<-glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,family=binomial,data=bankloan)

summary(riskmodel)

index<-createDataPartition(bankloan$DEFAULTER,p=0.7,list=FALSE)
traindata<-bankloan[index,]  

traindata$predprob<-predict(riskmodel,traindata,type='response')  
traindata$predY<-ifelse(traindata$predprob>0.30,1,0)

confusionMatrix(traindata$predY,traindata$DEFAULTER,positive="1")
