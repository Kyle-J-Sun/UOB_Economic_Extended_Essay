# Script Name: svmreturn.R
# Author: Jingkai Sun (ks3020@ic.ac.uk)

# predicting daily HSI data using SVM

#--------------------------------Loading Data----------------------------------------#
fileName = "HSID.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
fileLocation = file.path(defaultDataDir, fileName)
svmhsi = read.csv(fileLocation, header = T)
dim(svmhsi)

#-----------------------------Loading Libraries-------------------------------------#
library(tidyverse)
library(e1071)
library(ggplot2)

library(caret)
library(kernlab)
library(lattice)

#-----------------------------define train and test data-------------------------------------#
train_svmhsi = data.frame(svmhsi[,1][1:1719], scale(svmhsi[,4][1:1719]), svmhsi[,3][1:1719])
test_svmhsi = data.frame(svmhsi[,1][1720:2457], scale(svmhsi[,4][1720:2457]), svmhsi[,3][1720:2457])
col.headings = c('Date', 'return', 'Days')
names(train_svmhsi) = col.headings
names(test_svmhsi) = col.headings
col.headingss = c('Date', 'close', 'Days', 'Return')
names(svmhsi) = col.headingss

#----------------------------- fitting SVM-------------------------------------#

regressor = svm(formula = return ~ Days, data = train_svmhsi, type = 'classification', kernel = 'radial', cost = 1, sigma = 26.34877)
summary(regressor)

regressor2 = svm(formula = return ~ Days, data = train_svmhsi, type = 'classification', kernel = 'polynomial', cost = 50, degree = 3)
summary(regressor)

regressor3 = svm(formula = return ~ Days, data = train_svmhsi, type = 'classification', kernel = 'linear', cost = 1)
summary(regressor)

#-----------------------------Prediction of SVM-------------------------------------#
y_pred1 = predict(regressor, test_svmhsi)
y_pred1
y_pred2 = predict(regressor2,test_svmhsi)
y_pred2
y_pred3 = predict(regressor3,test_svmhsi)
y_pred3
table1 = data.frame(test_svmhsi$return, y_pred1, test_svmhsi$Days)
table2 = data.frame(test_svmhsi$return, y_pred2, test_svmhsi$Days)
table3 = data.frame(test_svmhsi$return, y_pred3, test_svmhsi$Days)
col.headings = c('Actual Return', 'Forecasted Return', 'Days')
names(table1) = col.headings
names(table2) = col.headings
names(table3) = col.headings



#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  geom_line(aes(x = table1$Days, y = table1$`Actual Return`), colour = 'red') +
  geom_line(aes(x = table1$Days, y = table1$`Forecasted Return`), colour = 'blue') +
  geom_line(aes(x = table2$Days, y = table2$`Forecasted Return`), colour = 'yellow') +
  geom_line(aes(x = table3$Days, y = table3$`Forecasted Return`), colour = 'green') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')

#------------------------------Finding Mean Percentage Error (MPE)-------------------------------#
#Radial
attach(table1)
percentage.error1 = ((table1$`Actual Price`-table1$`Forecasted Price`)/(table1$`Actual Price`))
percentage.error1
mean(percentage.error1) 
#1% for C = 1
#1.2% for C = 50

#Polynomial
attach(table2)
percentage.error2 = ((table2$`Actual Price`-table2$`Forecasted Price`)/(table2$`Actual Price`))
percentage.error2
mean(percentage.error2)  
#9.83 for C = 1 and Degree = 3
#9.81% for C = 50 and Degree = 3


#Linear
attach(table3)
percentage.error3 = ((table3$`Actual Price`-table3$`Forecasted Price`)/(table3$`Actual Price`))
percentage.error3
mean(percentage.error3) 
#13.67% for C = 1
#13.67% for C = 50

write.csv(table1, '/Users/kyle/Documents/Economics Extended Essay/Data/HSI/table1.csv')


#-----------------------------CV for SVM-------------------------------------#
control = trainControl(method = 'repeatedcv', number = 50, repeats = 3)
model = train(close ~ Days, data = train_svmhsi, method = 'svmRadialCost', trControl = control)
print(model)
plot(model) #cost = 1

control2 = trainControl(method = 'repeatedcv', number = 50, repeats = 3)
model2 = train(close ~ Days, data = train_svmhsi, method = 'svmRadialSigma', trControl = control2)
print(model2)
plot(model2) #The final values used for the model were sigma = 33.99872 and C = 1.

control4 = trainControl(method = 'repeatedcv', number = 50, repeats = 3)
model4 = train(close ~ Days, data = train_svmhsi, method = 'svmPoly', trControl = control4)
print(model4)
plot(model4) #The final values used for the model were degree = 3, scale = 0.1 and C = 1


#----------------------------- Corss-Validation -------------------------------------#
#Linear
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="linear" ,ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(tune.out) 
bestmod = tune.out$best.model
summary(bestmod)
#C = 5 for 60% train 40% test; 2016-2020 (4yrs)
#C = 1 for 70% train 30% test; 2017-2020 (3yrs)
#C =   for 80% train 20% test; 2018-2020 (2yrs)

#Polynomial
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="poly" ,ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
#C = 0.001 & Degree = 3 for 60% train 40% test; 2016-2020 (4yrs)
#C = 5 & Degree = 3 for 70% train 30% test; 2017-2020 (3yrs)
#C =    for 80% train 20% test; 2018-2020 (2yrs)

#Gausian Radial
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="radial" ,ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out) 
bestmod = tune.out$best.model
summary(bestmod)
#C = 100 for 60% train 40% test; 2016-2020 (4yrs)
#C = 100 for 70% train 30% test; 2017-2020 (3yrs)
#C =   for 80% train 20% test; 2018-2020 (2yrs)

#-----------------------------------------Questions-------------------------------------------------#
#What's the 'number' and 'repeats' meaning in CV???
#What's the 'sigma' meaning in CV???
#Waht's the 'degree' and 'scale' meaning???
#What's trainControl???
#Polynomial and linear SVR???
#Any other value that can test model accuracy???