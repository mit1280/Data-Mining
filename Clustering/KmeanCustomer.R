library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(GGally)
mydata <- read.csv(file = './customer.csv')

#remove Customer ID from dataframe
mydata2 <- mydata[mydata$CustomerID != '', ]
#mydata$InvoiceDateTime <- strftime(mydata$InvoiceDateTime, format="%H:%M")
#outlier
ggpairs(mydata2, upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "customer before outlier removal")

par(mfrow=c(1,2))
boxplot(mydata$TOTAL_QUANTITY, plot=TRUE, main="Total Quantity with Outliers", xlab="Total Quantity", ylab="Frequency")
outliers <- boxplot(mydata$TOTAL_QUANTITY, plot=FALSE)$out
mydata=mydata[-which(mydata$TOTAL_QUANTITY %in% outliers),]
boxplot(mydata$TOTAL_QUANTITY, plot=TRUE, main="Total Quantity without Outliers", xlab="Total Quantity", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$DISTINCT_PRODUCT, plot=TRUE,main="Distinct Product with Outliers", xlab="Distinct Product", ylab="Frequency")
outliers <- boxplot(mydata$DISTINCT_PRODUCT, plot=FALSE)$out
mydata=mydata[-which(mydata$DISTINCT_PRODUCT %in% outliers),]
boxplot(mydata$DISTINCT_PRODUCT, plot=TRUE, main="Distinct Product without Outliers", xlab="Distinct Product", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$REVENUES, plot=TRUE, main="Revenues with Outliers", xlab="Revenues", ylab="Frequency")
outliers <- boxplot(mydata$REVENUES, plot=FALSE)$out
mydata=mydata[-which(mydata$REVENUES %in% outliers),]
boxplot(mydata$REVENUES, plot=TRUE, main="Revenues without Outliers", xlab="Revenues", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$NUM_VISITS, plot=TRUE,main="Number of Visits with Outliers", xlab="Number of Visits", ylab="Frequency")
outliers <- boxplot(mydata$NUM_VISITS, plot=FALSE)$out
mydata=mydata[-which(mydata$NUM_VISITS %in% outliers),]
boxplot(mydata$NUM_VISITS, plot=TRUE, main="Number of Visits without Outliers", xlab="Number of Visits", ylab="Frequency")

par(mfrow=c(1,2))
boxplot(mydata$avg_price_item, plot=TRUE,main="Avg Price with Outliers", xlab="Avg Price", ylab="Frequency")
outliers <- boxplot(mydata$avg_price_item, plot=FALSE)$out
mydata=mydata[-which(mydata$avg_price_item %in% outliers),]
boxplot(mydata$avg_price_item, plot=TRUE, main="Avg Price without Outliers", xlab="Avg Price", ylab="Frequency")

#ignore cutomer ID
mydata2 <- mydata[2:(length(mydata)) ]
ggpairs(mydata2, upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "customer after outlier removal")

#mydata.scale = scale(mydata2)

withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}        

plot(withinSSrange(mydata2,1,30,150)) 

km=kmeans((mydata2),5,150)
autoplot(km,(mydata), frame=TRUE)
km$centers

clusteredval=cbind(mydata,km$cluster)
plot(clusteredval[,1:5],col=km$cluster)
write.csv(clusteredval, file = './customer2.csv',col.names = FALSE)

count(subset(clusteredval, km$cluster==1))
count(subset(clusteredval, km$cluster==2))
count(subset(clusteredval, km$cluster==3))
count(subset(clusteredval, km$cluster==4))
count(subset(clusteredval, km$cluster==5))
