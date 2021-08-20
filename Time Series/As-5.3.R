t=read.table("thirdproduct.csv",header=T)
head(t)
tSeries = ts(t[,1],freq=7)
head(tSeries)
plot(tSeries)
plot(stl(log(tSeries),s.window="periodic"))
t1<-log(tSeries)
plot(t1)


library(forecast)
hw = ets(tSeries,model="MAM")
mean(100*abs(fitted(hw) - tSeries)/tSeries)
# Your Holt-Winters error

ar <- Arima(tSeries,order=c(7,0,7))
mean(100*abs(fitted(ar) - tSeries)/tSeries)
# Your Arima error

library(caret)

xy=read.table("thirdproduct_hs.csv",sep=',',header=F)
y=xy[,8]
head(y)
x=xy[,1:7]

# Using pre-sliced data
myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)

# Linear regression
glmFitTime <- train(V8 ~ .,
                    data = xy,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
glmFitTime
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = x)
mean(100*abs(y_hat-y)/y)

# Support Vector Regression
svmFitTime <- train(V8 ~ .,
                    data = xy,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = x)
mean(100*abs(y_hat-y)/y)
# Your error with support vector regression

# Neural Network
nnFitTime <- train(V8 ~ .,
                   data = xy,
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   trControl = myCvControl,
                   tuneLength = 10,
                   linout = T,
                   trace = F,
                   MaxNWts = 10 * (ncol(xy) + 1) + 10 + 1,
                   maxit = 500)
nnFitTime
summary(nnFitTime)
y_hat = predict(nnFitTime, newdata = x)
mean(100*abs(y_hat-y)/y)

# Compare models
resamps <- resamples(list(lm = glmFitTime,
                          svn = svmFitTime,
                          nn = nnFitTime))
summary(resamps)
