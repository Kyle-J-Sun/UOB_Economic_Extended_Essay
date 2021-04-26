#--------------------------------Loading Data----------------------------------------#
fileName = "HSIDD.csv"
defaultDataDir = "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/"
fileLocation = file.path(defaultDataDir, fileName)
hsidd = read.csv(fileLocation, header = T)
dim(hsidd)


#-----------------------------Loading Libraries-------------------------------------#
library(tidyverse)
library(e1071)
library(ggplot2)

library(caret)
library(kernlab)
library(lattice)

#-----------------------------define train and test data for 70%-------------------------------------#
train_hsidd_70 = data.frame(log(hsidd[,6][1:241]), hsidd[,2][1:241], scale(hsidd[,3][1:241]), hsidd[,4][1:241], hsidd[,7][1:241], hsidd[,8][1:241], hsidd[,9][1:241], hsidd[,10][1:241])
col.headings = c('Close', 'date', 'DateID','Month', 'SMA', 'CCI', 'ROC', 'RSI')
names(train_hsidd_70) = col.headings

test_hsidd_70 = data.frame(log(hsidd[,6][242:349]), hsidd[,2][242:349], scale(hsidd[,3][242:349]), hsidd[,4][242:349], hsidd[,7][242:349], hsidd[,8][242:349], hsidd[,9][242:349], hsidd[,10][242:349])
names(test_hsidd_70) = col.headings


#----------------------------- fitting SVM-------------------------------------#
regressor_rad = svm(formula = Close ~ SMA + RSI, data = train_hsidd_70, type = 'eps-regression', kernel = 'radial', cost = 1, sigma = 0.3846438)
regressor_poly = svm(formula = Close ~ SMA + RSI, data = train_hsidd_70, type = 'eps-regression', kernel = 'polynomial', cost = 1, degree = 2)
regressor_lin = svm(formula = Close ~ SMA + RSI, data = train_hsidd_70, type = 'eps-regression', kernel = 'linear', cost = 1)
summary(regressor_lin)
summary(regressor_poly)
summary(regressor_rad)

#-----------------------------Prediction of SVM-------------------------------------#
predictor_lin = predict(regressor_lin, test_hsidd_70)
predictor_poly = predict(regressor_poly,test_hsidd_70)
predictor_rad = predict(regressor_rad,test_hsidd_70)

table_lin = data.frame(exp(test_hsidd_70$Close), exp(predictor_lin), test_hsidd_70$date)
table_poly = data.frame(exp(test_hsidd_70$Close), exp(predictor_poly), test_hsidd_70$date)
table_rad = data.frame(exp(test_hsidd_70$Close), exp(predictor_rad), test_hsidd_70$date)
col.headings = c('Actual Price', 'Forecasted Price', 'Date')
names(table_lin) = col.headings
names(table_poly) = col.headings
names(table_rad) = col.headings

#-----------------------------Visualiazation-------------------------------------#
ggplot() + 
  #geom_line(aes(x = hsidd$Date2[1:241], y = hsidd$Close[1:241]), colour = 'red') +
  geom_line(aes(x = hsidd$Date2[242:349], y = hsidd$Close[242:349]), colour = 'orange') +
  geom_line(aes(x = table_lin$Date, y = table_lin$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = table_poly$Date, y = table_poly$`Forecasted Price`), colour = 'grey') +
  geom_line(aes(x = table_rad$Date, y = table_rad$`Forecasted Price`), colour = 'green') +
  ggtitle('Prediction of HSI for 3 years') +
  xlab('Days') +
  ylab('HSI closing price')

#------------------------------ MAPE -------------------------------#
MAPE_rad = mean(abs((table_rad$`Actual Price`- table_rad$`Forecasted Price`)/table_rad$`Actual Price`)) 
MAPE_poly = mean(abs((table_poly$`Actual Price`- table_poly$`Forecasted Price`)/table_poly$`Actual Price`))
MAPE_lin = mean(abs((table_lin$`Actual Price`- table_lin$`Forecasted Price`)/table_lin$`Actual Price`))

#------------------------------ Finding RMSE -------------------------------#
RMSE_rad = RMSE(table_rad$`Actual Price`, table_rad$`Forecasted Price`) 
RMSE_poly = RMSE(table_poly$`Actual Price`, table_poly$`Forecasted Price`) 
RMSE_lin = RMSE(table_lin$`Actual Price`, table_lin$`Forecasted Price`) 

#------------------------------ Finding MAE -------------------------------#
MAE_rad = MAE(table_rad$`Actual Price`, table_rad$`Forecasted Price`)
MAE_poly = MAE(table_poly$`Actual Price`, table_poly$`Forecasted Price`)
MAE_lin = MAE(table_lin$`Actual Price`, table_lin$`Forecasted Price`)

#-----------------------------CV-------------------------------------#

control1 = trainControl(method = 'repeatedcv', number = 30, repeats = 5)
model1 = train(Close ~ SMA + RSI, data = train_hsidd_70, method = 'svmRadialSigma', trControl = control1)
print(model1)
plot(model1) 

control2 = trainControl(method = 'repeatedcv', number = 30, repeats = 5)
model2 = train(Close ~ SMA + RSI, data = train_hsidd_70, method = 'svmPoly', trControl = control2)
print(model2)
plot(model2) 

control3 = trainControl(method = 'repeatedcv', number = 30, repeats = 5)
model3 = train(Close ~ SMA + RSI, data = train_hsidd_70, method = 'svmLinear2', trControl = control2)
print(model3)
plot(model3) 

tune.out=tune(svm ,Close ~ SMA + RSI, data=train_hsidd_70, kernel ="radial" ,ranges = list(gamma = 0.1:10, cost = 0.001:10))
print(tune.out)
plot(tune.out)
#summary(tune.out) 
bestmod = tune.out$best.model

tune.out=tune(svm ,Close ~ SMA + RSI, data=train_hsidd_70, kernel ="linear" ,ranges = list(cost = 0.001:10))
print(tune.out)
plot(tune.out)
#summary(tune.out) 
bestmod = tune.out$best.model

tune.out=tune(svm ,Close ~ SMA + RSI,data=train_hsidd_70, kernel ="poly" ,ranges =list(cost= 0.001:10))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

