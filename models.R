library(caret)
library(stats)
library(leaps)
library(elasticnet)
library(LiblineaR)
library(pls)
library(neuralnet)

selectedFeatures=c("AvgSat","GradRate")
possibleFeatures=names(trainData[4:31])
possibleFeatures<- possibleFeatures[-19] # Omit the string values
training=trainData[,selectedFeatures]

##Linear Regression Model
modelloop=train(GradRate ~.,training,method="lm")

