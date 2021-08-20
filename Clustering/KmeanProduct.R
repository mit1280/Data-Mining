#install.packages('stats')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('ggfortify')
#install.packages('GGally')

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(GGally)

mydata <- read.csv(file = './product.csv')
mydata2=mydata[1:(length(mydata)-1)]
ggpairs(mydata2, upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Product before outlier removal")

par(mfrow=c(1,2))
#outlier
boxplot(mydata$DISTINCT_CUSTOMER, plot=TRUE, main="Distinct Customer with Outliers", xlab="Distinct Customer", ylab="Frequency")
outliers <- boxplot(mydata$DISTINCT_CUSTOMER, plot=FALSE)$out
mydata=mydata[-which(mydata$DISTINCT_CUSTOMER %in% outliers),]
boxplot(mydata$DISTINCT_CUSTOMER, plot=TRUE, main="Distinct Customer without Outliers", xlab="Distinct Customer", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$REVENUES, plot=TRUE,main="Revenues with Outliers", xlab="Revenues", ylab="Frequency")
outliers <- boxplot(mydata$REVENUES, plot=FALSE)$out
mydata=mydata[-which(mydata$REVENUES %in% outliers),]
boxplot(mydata$REVENUES, plot=TRUE, main="Revenues without Outliers", xlab="Revenues", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$NUM_VISITS, plot=TRUE,main="Number of visits with Outliers", xlab="Number of visits", ylab="Frequency")
outliers <- boxplot(mydata$NUM_VISITS, plot=FALSE)$out
mydata=mydata[-which(mydata$NUM_VISITS %in% outliers),]
boxplot(mydata$NUM_VISITS, plot=TRUE, main="Number of visits without Outliers", xlab="Number of visits", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$AVG_QUANTITY, plot=TRUE,main="Avg Quantity with Outliers", xlab="Avg Quantity", ylab="Frequency")
outliers <- boxplot(mydata$AVG_QUANTITY, plot=FALSE)$out
mydata=mydata[-which(mydata$AVG_QUANTITY %in% outliers),]
boxplot(mydata$AVG_QUANTITY, plot=TRUE, main="Avg Quantity without Outliers", xlab="Avg Quantity", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$AVG_UNITPRICE, plot=TRUE,main="Avg Price with Outliers", xlab="Avg Price", ylab="Frequency")
outliers <- boxplot(mydata$AVG_UNITPRICE, plot=FALSE)$out
mydata=mydata[-which(mydata$AVG_UNITPRICE %in% outliers),]
boxplot(mydata$AVG_UNITPRICE, plot=TRUE, main="Avg Price without Outliers", xlab="Avg Price", ylab="Frequency")

mydata2=mydata[1:(length(mydata)-1)]
ggpairs(mydata2, upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Product after outlier removal")

#mydata.scale = scale(mydata2)
km=kmeans(mydata2,6,150)
#autoplot(km,mydata.scale, frame=TRUE)
#km$centers

withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}        

plot(withinSSrange(mydata2,1,50,150)) 

clusteredval=cbind(mydata2,km$cluster)
write.csv(clusteredval, file = './product2.csv', col.names = "FALSE")
plot(clusteredval[,1:5],col=km$cluster)

count(subset(clusteredval, km$cluster==1))
count(subset(clusteredval, km$cluster==2))
count(subset(clusteredval, km$cluster==3))
count(subset(clusteredval, km$cluster==4))
count(subset(clusteredval, km$cluster==5))
count(subset(clusteredval, km$cluster==6))