library(caret)
library(randomForest)
sonarData=read.csv("car.csv",header=T)
train <- sample(nrow(sonarData), 0.8*nrow(sonarData), replace = FALSE)
TrainSet <- sonarData[train,]
ValidSet <- sonarData[-train,]
summary(TrainSet)
summary(ValidSet)
set.seed(7)

# Pick best "mtry" value for Random forest
a=c()
p=2
max=0
for (i in 3:8) {
  model3 <- randomForest(as.factor(shouldBuy) ~ price+maintenance+safety+seats+storage, data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$shouldBuy)
  if(max<a[i-2]){
    max=a[i-2]
    p=i
  }
}
plot(3:8,a ,xlab="Number of variables for splitting",ylab = "Accuracy")


model1 <- randomForest(as.factor(shouldBuy) ~ price+maintenance+safety+seats+storage, data = TrainSet, ntree = 500, mtry = p, importance = TRUE)

predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
treeCM=table(predValid,ValidSet$shouldBuy)
treeCM
print(paste("accuracy",sum(diag(treeCM))/sum(treeCM),sep=" "))
treeCM=as.data.frame.matrix(table(predValid,ValidSet$shouldBuy))
for(i in colnames(treeCM)){
  print(paste("Recall for", i," is",treeCM[i,i]/sum(treeCM[i]),sep=" "))
  
}

predValid <- predict(model1, ValidSet, type = "prob")

par(mfrow=c(2,2))
plot(roc(ValidSet[,7],predValid[,1]),main="ACC",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predValid[,2]),main="GOOD",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predValid[,3]),main="UNACC",xlab="Specificity",ylab="Sensitivity")
plot(roc(ValidSet[,7],predValid[,4]),main="VGOOD",xlab="Specificity",ylab="Sensitivity")


