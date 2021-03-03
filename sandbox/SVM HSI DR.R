#--------------------------------Loading Data----------------------------------------#
fileName = "HSIDR.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
fileLocation = file.path(defaultDataDir, fileName)
hsir = read.csv(fileLocation, header = T)
dim(hsir)

#-----------------------------Loading Libraries-------------------------------------#
library(tidyverse)
library(e1071)
library(ggplot2)

library(caret)
library(kernlab)
library(lattice)
#-----------------------------define train and test data-------------------------------------#
train_hsir = hsir[0:1719,1:10]
test_hsir = hsir[1720:2451,1:10]

#----------------------------- fitting SVM-------------------------------------#
regressor = svm(formula = return ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = hsir, type = 'eps-regression', kernel = 'radial', cost = 0.25, sigma = 0.04642674)
summary(regressor)

#-----------------------------Prediction of SVM-------------------------------------#
y_pred = predict(regressor, test_hsir)
y_pred

table = data.frame(test_hsir$return, y_pred)

#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  geom_line(aes(x = test_hsir$t, y = test_hsir$return), colour = 'red') +
  geom_line(aes(x = test_hsir$t, y = predict(regressor, newdata = test_hsir)), colour = 'blue') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Percentage Return')

#-----------------------------CV for SVM-------------------------------------#
control = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model = train(return ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = train_hsir, method = 'svmRadialCost', trControl = control)
print(model)
plot(model) #cost = 0.25

control2 = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model2 = train(return ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = train_hsir, method = 'svmRadialSigma', trControl = control)
print(model2)
plot(model2) #The final values used for the model were sigma = 0.04642674 and C = 0.25.

control3 = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model3 = train(return ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = train_hsir, method = 'svmRadial', trControl = control)
print(model3)
plot(model3) #C = 0.25

#Comparing with SVMPoly
control4 = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model4 = train(return ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = train_hsir, method = 'svmPoly', trControl = control)
print(model4)
plot(model4) #The final values used for the model were degree = 1, scale = 0.001 and C= 0.25.

#-----------------------------------Accuracy----------------------------------------#
write.csv(table, file = '/Users/kyle/Documents/Economics Extended Essay/Data/HSI/HSIRRR.csv')
