 
library(readxl)
#mydata <-read_excel('complete_mortal.xls', sheet=1,na="NA")
library(xtable)
library(flextable)
library(tidyverse)
library(reshape2)
library(officer)
library(Amelia)
library(missForest)
stat <- summary(mydata)
stat
tt = xtable_to_flextable(xtable(stat))
tt

mydata1 <- as.data.frame(mydata)

require(mice)
md.pattern(mydata1)
require("VIM")

impute <- function(x, x.impute){ifelse(is.na(x),x.impute,x)}
impute(mydata1, mean(mydata1))  
mydata2 <- mydata1[,-c(1:4)]
tempData <- mice(mydata2,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,action = 5)
 
cvdtotal <- completedData[,c(7,12:26)]
cvdtotal <- cvdtotal[,c(2,7:16)]
cvdall_lm <-  lm(cvdtotal$CVD_total_mortality ~.,data=cvdtotal)
model <- summary(cvdall_lm)
model
table_model <- xtable_to_flextable(xtable(model))
table_model
aov <- summary(aov(cvdall_lm))
table_aov <- xtable_to_flextable(xtable(aov))
table_aov


set.seed(20180808) 
index <- sample(2,nrow(cvdtotal),replace = TRUE,prob=c(0.75,0.25))
traindata <- cvdtotal[index==1,]
testdata <- cvdtotal[index==2,]


library(pROC)
library(kknn) 
wine_knn <- kknn(CVD_total_mortality~., traindata,testdata, k=7,distance = 2)
pre_knn <- fitted(wine_knn)
preds <- as.numeric(pre_knn)


library(e1071)
library(class) 
svmmodel <- svm(CVD_total_mortality ~.,data=traindata)
x <- traindata[,-1]
svm.pred <- predict(svmmodel,traindata[,-1])
dattt <-  traindata[,1]
length(dattt)
length(svm.pred)
######### 
preds <- svm.pred
actual <- dattt
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq#  
MAE <-   mean(abs(svm.pred - dattt));MAE  #  
MSE <- mean((svm.pred - dattt)^2);MSE #

RMSE <- sqrt(MSE);RMSE #
library(pROC)
modelroc_svm <- multiclass.roc(dattt,svm.pred)
roc.multi <- modelroc_svm
auc(roc.multi)

##################### 
library(rpart)
library(rpart.plot)
m.rpart <- rpart(CVD_total_mortality ~.,data=traindata)
summary(m.rpart)
rpart.plot(m.rpart)
dt.pred <- predict(m.rpart,traindata[,-1])
##################### 
preds <- dt.pred
actual <- dattt
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq
MAE <-   mean(abs(dt.pred - dattt));MAE  
MSE <- mean((dt.pred - dattt)^2);MSE
RMSE <- sqrt(MSE);RMSE 
modelroc_dt <- multiclass.roc(dattt,dt.pred)
roc.multi <- modelroc_dt
auc(roc.multi)
############ 
modelJIANDAN <-  lm(CVD_total_mortality ~.,data=traindata)
summary(model)
step(modelJIANDAN)

pred.lm <- predict(modelJIANDAN ,traindata[,-1])
############# 
preds <- pred.lm
actual <- dattt
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq
MAE <-   mean(abs(pred.lm - dattt));MAE 
MSE <- mean((pred.lm - dattt)^2);MSE 
RMSE <- sqrt(MSE);RMSE 
############### 2nd method ####################
library(pROC)
modelroc_lm <- multiclass.roc(dattt,pred.lm)
roc.multi <- modelroc_lm
auc(roc.multi)
 #########################

lmMiddle <- lm( CVD_total_mortality ~  Traffic.volume +Traffic.volume *Traffic.volume 
                +NOx + PM2.5 + Mean.air.temperature + Maximum.air.temperature 
                + Minimum.air.temperature + Heating.degree.days + Vapour.pressure
                + Average.of.mean.wind.speed.from.main.obs + Mean.wind.gust
              +NOx*Mean.air.temperature +NOx*Maximum.air.temperature 
                + NOx*Minimum.air.temperature  +NOx*Heating.degree.days+  NOx*Vapour.pressure 
                +NOx*Average.of.mean.wind.speed.from.main.obs + NOx*Mean.wind.gust +PM2.5*Mean.air.temperature
                +PM2.5*Maximum.air.temperature +PM2.5*Minimum.air.temperature  +PM2.5*Heating.degree.days 
                +PM2.5*Vapour.pressure  +PM2.5*Average.of.mean.wind.speed.from.main.obs  +PM2.5*Mean.wind.gust 
                +NOx* NOx  + PM2.5 * PM2.5 +Mean.air.temperature* Mean.air.temperature 
                + Maximum.air.temperature* Maximum.air.temperature 
                + Minimum.air.temperature* Minimum.air.temperature +Heating.degree.days* Heating.degree.days+ 
                  Vapour.pressure* Vapour.pressure  +Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs 
                + Mean.wind.gust* Mean.wind.gust  +NOx* NOx  * NOx    +PM2.5* PM2.5 * PM2.5  
                +Mean.air.temperature* Mean.air.temperature* Mean.air.temperature  +Maximum.air.temperature* Maximum.air.temperature* Maximum.air.temperature 
                +Minimum.air.temperature* Minimum.air.temperature* Minimum.air.temperature 
                +Heating.degree.days* Heating.degree.days* Heating.degree.days + Vapour.pressure* Vapour.pressure* Vapour.pressure+ 
                  Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs
                + Mean.wind.gust* Mean.wind.gust* Mean.wind.gust, data = traindata)
 summary(lmMiddle)
 

pred.lmMidd <- predict(lmMiddle ,traindata[,-1])
############# 
preds <- pred.lmMidd
actual <- dattt
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq#
MAE <-   mean(abs(pred.lmMidd - dattt));MAE 
MSE <- mean((pred.lmMidd - dattt)^2);MSE 
RMSE <- sqrt(MSE);RMSE #
############### 2nd method ####################
library(pROC)
modelroc_lm <- multiclass.roc(dattt,pred.lmMidd)
roc.multi <- modelroc_lm
auc(roc.multi)




mydata <-read_excel('complete_mortal - Copy.xls', sheet=1,na="NA")
cvdtotal <- mydata[,c(15,20:50)]


mydata1 <- as.data.frame(cvdtotal)
require(mice)
md.pattern(mydata1)
require("VIM")

impute <- function(x, x.impute){ifelse(is.na(x),x.impute,x)}
impute(mydata1, mean(mydata1))  
tempData <- mice(mydata1,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,action = 5)
cvdtotal <-completedData

set.seed(20180808) 
index <- sample(2,nrow(cvdtotal),replace = TRUE,prob=c(0.7,0.3))
traindata <- cvdtotal[index==1,]
testdata <- cvdtotal[index==2,]
dattt <-  traindata[,1]
summary(traindata)
###############
lmFUZA <- lm( CVD_total_mortality ~  Traffic.volume +Traffic.volume *Traffic.volume 
                +NOx + PM2.5 + Mean.air.temperature + Maximum.air.temperature 
                + Minimum.air.temperature + Heating.degree.days + Vapour.pressure
                + Average.of.mean.wind.speed.from.main.obs + Mean.wind.gust
                +NOx*Mean.air.temperature +NOx*Maximum.air.temperature 
                + NOx*Minimum.air.temperature  +NOx*Heating.degree.days+  NOx*Vapour.pressure 
                +NOx*Average.of.mean.wind.speed.from.main.obs + NOx*Mean.wind.gust +PM2.5*Mean.air.temperature
                +PM2.5*Maximum.air.temperature +PM2.5*Minimum.air.temperature  +PM2.5*Heating.degree.days 
                +PM2.5*Vapour.pressure  +PM2.5*Average.of.mean.wind.speed.from.main.obs  +PM2.5*Mean.wind.gust 
                +NOx* NOx  + PM2.5 * PM2.5 +Mean.air.temperature* Mean.air.temperature 
                + Maximum.air.temperature* Maximum.air.temperature 
                + Minimum.air.temperature* Minimum.air.temperature +Heating.degree.days* Heating.degree.days+ 
                  Vapour.pressure* Vapour.pressure  +Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs 
                + Mean.wind.gust* Mean.wind.gust  +NOx* NOx  * NOx    +PM2.5* PM2.5 * PM2.5  
                +Mean.air.temperature* Mean.air.temperature* Mean.air.temperature  +Maximum.air.temperature* Maximum.air.temperature* Maximum.air.temperature 
                +Minimum.air.temperature* Minimum.air.temperature* Minimum.air.temperature 
                +Heating.degree.days* Heating.degree.days* Heating.degree.days + Vapour.pressure* Vapour.pressure* Vapour.pressure+ 
                  Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs* Average.of.mean.wind.speed.from.main.obs
                + Mean.wind.gust* Mean.wind.gust* Mean.wind.gust+l3Mean.wind.gust+l2Mean.wind.gust+l1Mean.wind.gust+
                l3Average.of.mean.wind.speed.from.main.obs+l2Average.of.mean.wind.speed.from.main.obs+l1Average.of.mean.wind.speed.from.main.obs 
              +l3Vapour.pressure+l2Vapour.pressure+l1Vapour.pressure+l3Heating.degree.days+l2Heating.degree.days+l1Heating.degree.days+
                l3Minimum.air.temperature+l2Minimum.air.temperature+l1Minimum.air.temperature+l3Maximum.air.temperature+l2Maximum.air.temperature+
                l1Maximum.air.temperature+l3Mean.air.temperature+l2Mean.air.temperature+l1Mean.air.temperature+
                 Traffic.volume*NOx +Traffic.volume * PM2.5, data = traindata )
summary(lmFUZA)
pred.lmFUZA <- predict(lmFUZA ,traindata[,-1])
############# 
preds <- pred.lmFUZA
actual <- as.numeric(unlist(dattt))
length(preds)
length(actual)
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq 
MAE <-   mean(abs(pred.lmFUZA - dattt));MAE  
MSE <- mean((pred.lmFUZA - dattt)^2);MSE  
RMSE <- sqrt(MSE);RMSE  
############### 2nd method ####################
library(pROC)
modelroc_lm <- multiclass.roc(dattt,pred.lmFUZA)
roc.multi <- modelroc_lm
auc(roc.multi)


###########lag model ####
library(dplyr)
library(purrr)
mydata <-read_excel('complete_mortal - Copy.xls', sheet=1,na="NA")


lag_days <- 6
data <- mydata[,c(15,20:56)]
set.seed(20180808) 
index <- sample(2,nrow(data),replace = TRUE,prob=c(0.7,0.3))
traindata <- data[index==1,]
testdata <- data[index==2,]

lmLAG <- lm( CVD_total_mortality ~  Traffic.volume 
              +NOx + PM2.5 + Mean.air.temperature + Maximum.air.temperature 
              + Minimum.air.temperature + Heating.degree.days + Vapour.pressure
              + Average.of.mean.wind.speed.from.main.obs + Mean.wind.gust
              ++l3Mean.wind.gust+l2Mean.wind.gust+l1Mean.wind.gust+
                l3Average.of.mean.wind.speed.from.main.obs+l2Average.of.mean.wind.speed.from.main.obs+l1Average.of.mean.wind.speed.from.main.obs 
              +l3Vapour.pressure+l2Vapour.pressure+l1Vapour.pressure+l3Heating.degree.days+l2Heating.degree.days+l1Heating.degree.days+
                l3Minimum.air.temperature+l2Minimum.air.temperature+l1Minimum.air.temperature+l3Maximum.air.temperature+l2Maximum.air.temperature+
                l1Maximum.air.temperature+l3Mean.air.temperature+l2Mean.air.temperature
             +l1Mean.air.temperature, data = data )
summary(lmLAG)
lmLAG.pred <- predict(lmLAG ,traindata[,-1])
############# 
stat(lmLAG)
preds <- lmLAG.pred
actual <- as.numeric(unlist(dattt))[1:10252]
length(preds)
length(actual)
rss <- sum((preds - actual) ^ 2)  
tss <- sum((actual - mean(actual)) ^ 2)  
rsq <- 1 - rss/tss;rsq#
MAE <-   mean(abs(lmLAG.pred - actual));MAE # 
MSE <- mean((lmLAG.pred - actual)^2);MSE #
RMSE <- sqrt(MSE);RMSE # 
############### 2nd method ####################
library(pROC)
modelroc_lm <- multiclass.roc(actual,preds)
roc.multi <- modelroc_lm
auc(roc.multi)#



