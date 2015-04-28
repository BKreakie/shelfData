###Code for random forest on shelf data
##April 28, 2015


setwd("L:/Public/Betty/shelf_data/shelfData")

library(randomForest)

###reading in data
modernData<-read.csv(file="modern_04212015.csv",header=TRUE)

historicData<-read.csv(file="historic_04212015.csv",header=TRUE)


###randomForest models
##Removing NA from nitrogen
x<-complete.cases(modernData[,9])
modernN<-modernData[x,]

modernPredVar<-c(3,4,6,7,8,13,16,18:32)

x<-complete.cases(modernN[,modernPredVar])
###101 total points


modernN<-modernN[x,]

modernRF_1<-randomForest(modernN[,modernPredVar],modernN[,10],ntree=10000,importance=TRUE,proximity=TRUE,na.action=na.omit)

#> modernRF_1

#Call:
#  randomForest(x = modernN[, modernPredVar], y = modernN[, 10],      ntree = 10000, importance = TRUE, proximity = TRUE, na.action = na.omit) 
#Type of random forest: regression
#Number of trees: 10000
#No. of variables tried at each split: 7

#Mean of squared residuals: 2.728702
#% Var explained: 59.38


##With only surface points
modernN_surf<-subset(modernN,modernN$position=="Surf")
modernRF_2<-randomForest(modernN_surf[,modernPredVar],modernN_surf[,10],ntree=10000,importance=TRUE,proximity=TRUE,na.action=na.omit)




##Classification Tree on modern data
dataDim<-dim(modernN)

dNCat<-array("med",dataDim[1])

data<-cbind(modernN,dNCat)
x<-quantile(data[,9])

for(i in 1:dataDim[1]){
  if (data[i,9]<=x[2]){
    data[i,dataDim[2]+1]<-"low"
  }
  if(data[i,9]>=x[4]){
    data[i,dataDim[2]+1]<-"high"
  }
}



data[,dataDim[2]+1]<-factor(data[,dataDim[2]+1],levels=c("low","med","high"),ordered=TRUE)

modernRF_3<-randomForest(data[,modernPredVar],data[,33], ntree=10000,importance=TRUE,proximity=TRUE)

#> modernRF_3

#Call:
#  randomForest(x = data[, modernPredVar], y = data[, 33], ntree = 10000,      importance = TRUE, proximity = TRUE) 
#Type of random forest: classification
#Number of trees: 10000
#No. of variables tried at each split: 4

#OOB estimate of  error rate: 41.58%
#Confusion matrix:
#  low med high class.error
#low    8  17    1   0.6923077
#med   10  36    3   0.2653061
#high   3   8   15   0.4230769


importance(modernRF_3)
varImpPlot(modernRF_3)
