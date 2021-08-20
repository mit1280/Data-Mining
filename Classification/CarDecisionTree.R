library(rpart)
library(caret)
library(pROC)
library(rpart.plot)

sonarData=read.csv("car.csv",header=T)
sonarData <- sonarData[2:(length(sonarData)) ]
train <- sample(nrow(sonarData), 0.8*nrow(sonarData), replace = FALSE)
TrainSet <- sonarData[train,]
ValidSet <- sonarData[-train,]


#chi square test useful to find relationship between two categorical variables.
Carheader=colnames(sonarData)[which( colnames(sonarData)!="shouldBuy" )]
for (i in Carheader){
  x=chisq.test(sonarData[i],sonarData$shouldBuy)
  print(paste("p value for", toString(i), "is", toString(x$p.value),sep=" "))
  if(x$p.value>0.1){
    print(paste(i, "has less p-value so we'll drop",sep=" "))
  }
}
#choose depth of the Tree
acc=0
a=c()
depth=1
for (i in 2:10) {
  treeSonar = rpart(shouldBuy~price+maintenance+safety+seats+storage,data=TrainSet,method="class",parms=list(split='information'),maxdepth=i)
  predSonar=predict(treeSonar,newdata=ValidSet,type="class")
  treeCM=table(predSonar,ValidSet$shouldBuy)
  a[i-1]=sum(diag(treeCM))/sum(treeCM)
  if(acc<a[i-1]){
    acc=a[i-1]
    depth=i
  }
}
plot(2:10,a,xlab="Depth of tree",ylab = "Accuracy")

treeSonar = rpart(shouldBuy~.,data=TrainSet,method="class",parms=list(split='information'),maxdepth=8)
predSonar=predict(treeSonar,newdata=ValidSet,type="class")
treeCM=table(predSonar,ValidSet$shouldBuy)
treeCM
#how many correctly identify TP and TN
print(paste("accuracy",sum(diag(treeCM))/sum(treeCM),sep=" "))

#Correctly identify +ve from Actual +ve(TP+FN)
#Sensitivity
treeCM=as.data.frame.matrix(table(predSonar,ValidSet$shouldBuy))
for(i in colnames(treeCM)){
  print(paste("Recall for", i," is",treeCM[i,i]/sum(treeCM[i]),sep=" "))
  
}

rpart.plot(treeSonar)

predSonarProb = predict(treeSonar,newdata=ValidSet,type="prob")

par(mfrow=c(2,2))
#Specificity: Correctly identify -ve from Actual -ve(TN+FP)
plot(roc(ValidSet[,7],predSonarProb[,1]),main="ACC",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predSonarProb[,2]),main="GOOD",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predSonarProb[,3]),main="UNACC",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predSonarProb[,4]),main="VGOOD",xlab="Specificity",ylab="Sensitivity")

