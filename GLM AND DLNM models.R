library(dlnm)
library(splines)
library(tsModel)
library(ggplot2)
library(openair)
library(officer)
library(knitr)
library(dplyr)
library(purrr)
library(shiny)
set.seed(2014)
library(readxl)
library(descriptr)
library(corrgram)
library(mice)
library(reshape2)
library(openxlsx)
library(e1071)
library(rpart)
library(rpart.plot)
library(xlsx2dfs)
library(corrplot)
library(party)
library(randomForest)
library(vivid)
library(ranger)
library(vegan)
library(patchwork)
library(rfPermute)
library(export)
library(tidyverse)
library(broom.mixed)
library(ggpubr)
library(jtools)
library(urca)
library(forecast)
library(MASS)
library(gam)
library(rsq)
library(pROC)
###########################################
mydata1 <-read_excel('mydata1.xls', sheet=1,na="NA")

RAW <- as.matrix(mydata1[,c(18:37)])
for (j in 1:ncol(RAW)){
  RAW[,j] <- as.numeric(as.factor(RAW[,j]))
}
for (j in 1:ncol(RAW)){
  b <- class(RAW[,j])
  print(b)
}

for (j in 1:ncol(RAW)){
  RAW[,j] <- (RAW[,j]-min(RAW[,j]))/(max(RAW[,j])-min(RAW[,j]))
}
 
scale <- RAW
summary(RAW)

 library(mice)
imp1  <- mice(RAW, seed = 1234,Method = "logreg") 
comData <- complete(imp1,action = 1);
library(tseries)
mydata <- comData

##############################################################33
attach(mydata)
mydata2 <- cbind(mydata1[,13:17],mydata)
# Defining variables
Y <- mydata2$CVD_total_mortality
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

# Descriptive statistics and plotting the data
summary(Y)
summary(d.Y)

plot.ts(Y)
plot.ts(d.Y)

# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=6)
adf.test(Y, alternative="stationary")
adf.test(Y, alternative="explosive", k=0)
adf.test(d.Y,alternative="stationary", k=6)
# Augmented Dickey-Fuller test
adf.test(Y, alternative="stationary")
#Lag order = 14
# ACF and PACF
acf(Y)
pacf(Y)

png('acf plot.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)

 acf(Y)

dev.off()


png('pacf plot.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
 

pacf(Y)
LI
dev.off()
library(urca)
library("forecast")
skirtsarima<-auto.arima( Y,trace=T)    
#ARIMA(0,1,1) 

library(shiny)
model1 <- reactive({
  readRDS()
})
typeof(model1)

library("dlnm")
library("pacman")
library(dlm)
library("dlnm")
library(mgcv)
dateNum <- seq(as.Date("2009-01-01"), as.Date("2018-12-31"),by="day")
dateNum <- rep(dates,4)
dateNum <- dates[1:14606]
dates <- as.factor(dateNum)
dates <- dateNum
datNum <-  data.frame(dates,mydata1[,18:28])
dat <- data.frame(dates,mydata1[,18:28])
""
mylog <- function(x) log(x + 1)
# In Table 4, DLNM model
# create the crossbasis objects and summarize their contents
CVD_total_mortality <- crossbasis(dat$CVD_total_mortality, lag=6, argvar=list(fun="mylog"),
                  arglag=list(fun="poly",degree=2))
NOx <- crossbasis(dat$NOx, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Traffic.volume <- crossbasis(dat$Traffic.volume, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Mean.air.temperature <- crossbasis(dat$Mean.air.temperature, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
PM2.5 <- crossbasis(dat$PM25, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Maximum.air.temperature <- crossbasis(dat$Maximum.air.temperature, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Minimum.air.temperature <- crossbasis(dat$Minimum.air.temperature, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Heating.degree.days <- crossbasis(dat$Heating.degree.days, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Vapour.pressure <- crossbasis(dat$Vapour.pressure, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Average.of.mean.wind.speed.from.main.obs <- crossbasis(dat$Average.of.mean.wind.speed.from.main.obs, 
                     lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))
Mean.wind.gust <- crossbasis(mydata1$Mean.wind.gust, lag=6, argvar=list(fun="mylog"),
                      arglag=list(fun="poly",degree=2))

""
# create the crossbasis objects and summarize their contents
CVD_total_mortality <- crossbasis(traindata$CVD_total_mortality, lag=6, argvar=list(fun="mylog"),
                                  arglag=list(fun="poly",degree=2))
NOx <- crossbasis(traindata$NOx, lag=6, argvar=list(fun="mylog"),
                  arglag=list(fun="poly",degree=2))
Traffic.volume <- crossbasis(traindata$Traffic.volume, lag=6, argvar=list(fun="mylog"),
                             arglag=list(fun="poly",degree=2))
Mean.air.temperature <- crossbasis(traindata$Mean.air.temperature, lag=6, argvar=list(fun="mylog"),
                                   arglag=list(fun="poly",degree=2))
PM2.5 <- crossbasis(traindata$PM25, lag=6, argvar=list(fun="mylog"),
                    arglag=list(fun="poly",degree=2))
Maximum.air.temperature <- crossbasis(traindata$Maximum.air.temperature, lag=6, argvar=list(fun="mylog"),
                                      arglag=list(fun="poly",degree=2))
Minimum.air.temperature <- crossbasis(traindata$Minimum.air.temperature, lag=6, argvar=list(fun="mylog"),
                                      arglag=list(fun="poly",degree=2))
Heating.degree.days <- crossbasis(traindata$Heating.degree.days, lag=6, argvar=list(fun="mylog"),
                                  arglag=list(fun="poly",degree=2))
Vapour.pressure <- crossbasis(traindata$Vapour.pressure, lag=6, argvar=list(fun="mylog"),
                              arglag=list(fun="poly",degree=2))
Average.of.mean.wind.speed.from.main.obs <- crossbasis(traindata$Average.of.mean.wind.speed.from.main.obs, 
                                                       lag=6, argvar=list(fun="mylog"),
                                                       arglag=list(fun="poly",degree=2))
Mean.wind.gust <- crossbasis(traindata$Mean.wind.gust, lag=6, argvar=list(fun="mylog"),
                             arglag=list(fun="poly",degree=2))



mydata <- dat[, 1:11]
set.seed(20220808)
index <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.75, 0.25))
traindata <- mydata[index == 1, ]
testdata <- mydata[index == 2, ]
DLNM <- glm(traindata$CVD_total_mortality ~ .,
           family = quasipoisson(), data = traindata, control = glm.control(maxit = 100))

# model1 <- gam( CVD_total_mortality ~ NOx + Traffic.volume +Mean.air.temperature+Minimum.air.temperature
#                +Maximum.air.temperature+PM25+Heating.degree.days+Vapour.pressure
#                +Average.of.mean.wind.speed.from.main.obs+ Mean.wind.gust+dates[1:11008],
#                family=quasipoisson(), traindata)


SST <- sum((traindata$CVD_total_mortality - mean(traindata$CVD_total_mortality))^2)
SSR <- sum(residuals(DLNM)^2)
R_squared  <- 1 - SSR / SST
n <- nrow(traindata)  
p <- length(coefficients(DLNM)) - 1  
Adjusted_R_squared  <- 1 - (SSR / (n - p - 1)) / (SST / (n - 1))
Adjusted_R_squared
MSE <- SSR/(n-p);MSE 
RMSE <- sqrt(MSE);RMSE 
library(pROC)
modelroc_dlnm <- multiclass.roc(dattt,preds)
roc.multi <- modelroc_dlnm
auc(roc.multi)
############ creat interaction and lagged terms #################
df <- dat[,-c(1,2)]
dim(df)
# Multiply each pair of columns and add the result to the original dataset
result <- NULL
data<-df
 for (i in 1:(ncol(data) - 1)) {
  for (j in (i + 1):ncol(data)) {
     product <- data[, i] * data[, j]
     new_col_name <- paste0(names(data)[i], "_x_", names(data)[j])
     result <- cbind(result, product)
    colnames(result)[ncol(result)] <- new_col_name
  }
}
 
mydata <- cbind(dat[,c(2)],result)
mydata <- mydata[,-1]
for (j in 1:ncol(mydata)){
  mydata[,j] <- as.numeric(as.factor(mydata[,j]))
}
for (j in 1:ncol(mydata)){
  b <- class(mydata[,j])
  print(b)
}

for (j in 1:ncol(mydata)){
  mydata[,j] <- (mydata[,j]-min(mydata[,j]))/(max(mydata[,j])-min(mydata[,j]))
}
InteractDF <- mydata 
dim(InteractDF)
forlag <- scale[,-1]
forlag <- forlag[,1:10]
dim(forlag)
# Set the maximum number of lags to add
lag_count <- 6
forlag <- as.data.frame(forlag)
# Loop through each variable to add six lagged
for (col_name in colnames(forlag)) {
  for (lag in 1:lag_count) {
    new_col_name <- paste0(col_name, "_lag_", lag)
    forlag <- forlag %>%
      mutate(!!new_col_name := lag(forlag[[col_name]], lag))
  }
}

#Add lagged variables to the original dataset
df_with_lags <- bind_cols(dates,RAW[,1], forlag,InteractDF)
  colnames(df_with_lags)[2] <- "CVD_total_mortality"
 colnames(df_with_lags)[1] <- "time"
 df_117 <- df_with_lags[,1:117]
#################### Appendix 2, DLNM Have all interaction and have all lag########################################
DNM <- glm( df_with_lags$CVD_total_mortality ~ ., 
               family=quasipoisson(), data=df_with_lags, control = glm.control(maxit = 100))
 

SST <- sum((df_with_lags$CVD_total_mortality - mean(df_with_lags$CVD_total_mortality))^2)
 SSR <- sum(residuals(DNM)^2)
 R_squared  <- 1 - SSR / SST
R_squared
  
n <- nrow(df_with_lags)  
p <- length(coefficients(DNM)) - 1  
Adjusted_R_squared  <- 1 - (SSR / (n - p - 1)) / (SST / (n - 1))
Adjusted_R_squared
MSE <- SSR/(n-p);MSE 
RMSE <- sqrt(MSE);RMSE 
#################### train and test data ##################################################
mydata <- df_with_lags[,1:11]
set.seed(20180808)
index <- sample(2,nrow(mydata),replace = TRUE,prob=c(0.75,0.25))
traindata <- mydata[index==1,]
testdata <- mydata[index==2,]

DLNM <- glm( traindata$CVD_total_mortality ~ ., 
            family=quasipoisson(), data=traindata, control = glm.control(maxit = 100))
 

DLNM.pred <- predict(DLNM,traindata[,-1])
dattt <-  traindata[,1]
length(dattt)
length(DLNM.pred)
preds <- DLNM.pred
actual <- dattt
SST <- sum((dattt - mean(dattt))^2)
SST <- sum((df_with_lags$CVD_total_mortality - mean(df_with_lags$CVD_total_mortality))^2)
SSR1 <- sum(residuals(DNM)^2)
SSR1

R_squared  <- 1 - SSR1 / SST
R_squared
n <- nrow(traindata)  
p <- length(coefficients(DNM)) - 1  
Adjusted_R_squared  <- 1 - (SSR1 / (n - p - 1)) / (SST / (n - 1))
Adjusted_R_squared
MAE <-   mean(abs(preds - dattt));MAE  
MSE <- SSR1/(n-p);MSE 
RMSE <- sqrt(MSE);RMSE 
library(pROC)
mod <- multiclass.roc(dattt,preds)
roc.multi <- mod
auc(roc.multi)
#######################################
library(MASS)
residuals <- residuals(DNM)
mse <- sum(residuals^2) / length(residuals)
print(mse)
mae <- sum(abs(residuals)) / length(residuals)
print(mae)
rmse <- sqrt(mse)
print(rmse)
library(gam)
############Gam for table 4 ##########
modelGAMtrain <- gam(CVD_total_mortality ~ s(NOx) + s(Traffic.volume)+s(Mean.air.temperature)+s(Minimum.air.temperature)
                     +s(Maximum.air.temperature)+s(PM25)+s(Heating.degree.days)+s(Vapour.pressure)
                     +s(Average.of.mean.wind.speed.from.main.obs)+ s(Mean.wind.gust),
                     family=quasipoisson, traindata)


summary.aov(modelGAMtrain)
model1.pred <- predict(modelGAMtrain,traindata[,-1])
dattt <-  traindata[,1]
length(dattt)
length(model1.pred)

library(gam) 
dateNum <- seq(as.Date("2009-01-01"), as.Date("2018-12-31"),by="day")
dateNum <- rep(dates,4)
dateNum <- dates[1:14606]
dates <- as.factor(dateNum)
dates <- dateNum
datNum <-  data.frame(dates,mydata1[,18:28])
dat <- data.frame(dates,mydata1[,18:28])

predicted_values <- predict(modelGAMtrain)

correlation <- cor(predicted_values, modelGAMtrain$y)^2
r_squared <- correlation

n <- length(modelGAMtrain$y)
p <- length(modelGAMtrain$coefficients)
adj_r_squared <- 1 - ((1 - r_squared) * ((n - 1) / (n - p - 1)))

print(paste("R???:", round(r_squared, 4)))
print(paste("??????R???:", round(adj_r_squared, 4)))


SST <- sum((df_with_lags$CVD_total_mortality - mean(df_with_lags$CVD_total_mortality))^2)
SST
SSR <- sum(residuals(modelGAMtrain)^2)
SSR
R_squared  <- 1 - SSR / SST
R_squared

MSE <- SSR/(n-p);MSE 

RMSE <- sqrt(MSE);RMSE 
library(pROC)
modelroc_gam <- multiclass.roc(modelGAMtrain$y,predicted_values)
roc.multi <- modelroc_gam
auc(roc.multi)

#################### GAM full model #####################3
bootstrap_gam <- function(modelGAMFULL, df_with_lags, n_boot = 1000) {
  coefs <- coef(modelGAMFULL)
  coefs
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = length(coefs))
  for (i in 1:n_boot) {
    sampled_data <- df_with_lags[sample(nrow(df_with_lags), replace = TRUE), ]
    boot_model <- gam(modelGAMFULL$formula, data = sampled_data)
    boot_coefs[i, ] <- coef(boot_model)
  }
  boot_ci <- t(apply(boot_coefs, 2, function(x) quantile(x, c(0.025, 0.975))))
  colnames(boot_ci) <- c("lower", "upper")
  return(boot_ci)
}
boot_ci <- bootstrap_gam(modelGAMFULL, df_with_lags)
boot_ci 

y <-df_with_lags$CVD_total_mortality
modelGAMFULL  <- gam(y ~ .,family = quasipoisson, df_with_lags)
 
library(mgcv)

summary(modelGAMFULL)
SST <- sum((df_with_lags$CVD_total_mortality - mean(df_with_lags$CVD_total_mortality))^2)
SSR <- sum(residuals(modelGAMFULL)^2)
SSR
R_squared  <- 1 - SSR / SST
R_squared

n <- nrow(df_with_lags)  # 
p <- length(coefficients(modelGAMFULL)) - 1  
Adjusted_R_squared  <- 1 - (SSR / (n - p - 1)) / (SST / (n - 1))
Adjusted_R_squared

MSE <- SSR/(n-p);MSE

RMSE <- sqrt(MSE);RMSE 
library(pROC)
modelroc_svm <- multiclass.roc(dattt,preds)
roc.multi <- modelroc_svm
auc(roc.multi)

 coef_estimates <- coef(modelGAMFULL)
 print(coef_estimates)
 
 adj_r_squared <- summary(modelGAMFULL)$adj.r.squared
print(adj_r_squared)

 residuals <- residuals(modelGAMFULL)

 mse <- sum(residuals^2) / length(residuals)
print(mse)#

 mae <- sum(abs(residuals)) / length(residuals)
print(mae)#

rmse <- sqrt(mse)
print(rmse)#

  
##################Double check #############
set.seed(20180808) 
for (j in 1:ncol(df_117)){
  df_117[,j] <- as.numeric(as.character(df_117[,j]))
}
for (j in 1:ncol(df_117)){
  b <- class(df_117[,j])
}

dfNEW<-df_117[8:14606,-1]
as.matrix(dfNEW)
index <- sample(2,nrow(dfNEW),replace = TRUE,prob=c(0.7,0.3))
traindata <- dfNEW[index==1,]
testdata <- dfNEW[index==2,]

for (j in 1:ncol(traindata)){
  traindata[,j] <- as.numeric(as.character(traindata[,j]))
}
for (j in 1:ncol(traindata)){
  b <- class(traindata[,j])
  print(b)
}

######## dlnm #####
dlnm.pred <- predict(DNM,mat[,-c(1)])
dattt <-  traindata[,1]
actual <-  dattt 
preds <-  glm.pred 
rsq(model1)
rsq
library(rsq)
library(pROC)
modelroc_m <- multiclass.roc(dattt,dlnm.pred)
roc.multi <- modelroc_m
auc(roc.multi)


library(dplyr)
D_LAG <- completedData[,c(17:26)]
for (j in 1:ncol(D_LAG)){
  D_LAG[,j]<- as.numeric(as.factor(D_LAG[,j]))
}

for (j in 1:ncol(D_LAG)) {
  b <- class(D_LAG[,j])
  print(b)
}
as.numeric(D_LAG$NOx)
library(mgcv)


###################GAM#####

gam.pred <- predict(gam_poisson,traindata[,-1])

dattt <-  traindata[,1]
actual <-  dattt 
preds <-  gam.pred 
rsq(gam_poisson)
rsq#  
library(rsq)

MAE <-   mean(abs(gam.pred - dattt));MAE  # [1] 1.934304
MSE <- mean((gam.pred - dattt)^2);MSE
RMSE <- sqrt(MSE);RMSE#2.645999
library(pROC)


modelroc_m <- multiclass.roc(dattt,gam.pred)
roc.multi <- modelroc_m
auc(roc.multi)#
########Done
 
 

 