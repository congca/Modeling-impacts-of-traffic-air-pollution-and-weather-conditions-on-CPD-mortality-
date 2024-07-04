########## For DF/ADF test for each variables for each city##################3
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
library(splines)
library("readxl")
library(ggplot2)
library(descriptr)
library(corrgram) 
library(mice)
library(reshape2) 
library(openxlsx)
library(e1071)
library(rpart)
library(rpart.plot)
library(xlsx2dfs)
library("corrplot")
library(readxl)
library(party)
library("randomForest")
library(vivid) # for visualisations 
library(randomForest) # for model fit
library(ranger)       # for model fit
library(vegan)
library(patchwork)
library(rfPermute)
library(export)
library(tidyverse)
library(reshape2)
library(corrplot)
library(broom.mixed)
library(ggpubr)
library(jtools)
###########################################
mydata1 <-read_excel('mydata2.xls', sheet=1,na="NA")
mydata1 <-read_excel('Oslo.xls', sheet=1,na="NA")
mydata1 <-read_excel('Bergen.xls', sheet=1,na="NA")
mydata1 <-read_excel('Trondheim.xls', sheet=1,na="NA")
mydata1 <-read_excel('Tromso.xls', sheet=1,na="NA")


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

library(mice)
imp1  <- mice(RAW, seed = 1234,Method = "logreg") 
comData <- complete(imp1,action = 1);
library(tseries)
mydata <- comData
###########################################
attach(mydata)
mydata2 <- cbind(mydata1[,13:17],mydata[1:3651,])
 
# Defining variables
Y <- mydata2$CVD_total_mortality
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
#Lag order = 6
# Defining variables
Y <- mydata2$Traffic.volume
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
#Lag order = 6

# Defining variables
Y <- mydata2$NOx
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
# Defining variables
Y <- mydata2$PM25
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
# Defining variables
Y <- mydata2$Mean.air.temperature
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
# Defining variables
Y <- mydata2$Maximum.air.temperature
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
# Defining variables
Y <- mydata2$Minimum.air.temperature
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
# Defining variables
Y <- mydata2$Heating.degree.days
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y

adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)

Y <- mydata2$Vapour.pressure
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y
adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)

Y <- mydata2$Average.of.mean.wind.speed.from.main.obs
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y
adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)

Y <- mydata2$Mean.wind.gust
d.Y <- diff(Y)
t <- mydata2$yearqrt
dppi <- d.Y
adf.test(Y, alternative="stationary", k=6)
adf.test(d.Y,alternative="stationary", k=6)
################Done##################

