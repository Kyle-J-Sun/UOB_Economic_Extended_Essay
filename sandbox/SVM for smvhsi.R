# Script Name: SVM for smvhsi.R
# Author: Jingkai Sun (ks3020@ic.ac.uk)

# predicting daily HSI data using SVM

#--------------------------------Loading Data----------------------------------------
fileName = "HSIDD.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/NEW/"
fileLocation = file.path(defaultDataDir, fileName)
svmhsi = read.csv(fileLocation, header = T)
dim(svmhsi)

#-----------------------------Loading Libraries-------------------------------------
library(tidyverse)
library(e1071)
library(ggplot2)

library(caret)
library(kernlab)
library(lattice)

#-----------------------------define train and test data-------------------------------------
train70_svmhsi = data.frame(svmhsi[,1][1:1719], log(svmhsi[,2][1:1719]), svmhsi[,3][1:1719])
train80_svmhsi = data.frame(svmhsi[,1][1:1965], log(svmhsi[,2][1:1965]), svmhsi[,3][1:1965])
train90_svmhsi = data.frame(svmhsi[,1][1:2211], log(svmhsi[,2][1:2211]), svmhsi[,3][1:2211])
test3_svmhsi = data.frame(svmhsi[,1][1720:2457], log(svmhsi[,2][1720:2457]), svmhsi[,3][1720:2457])
test2_svmhsi =  data.frame(svmhsi[,1][1966:2211], log(svmhsi[,2][1966:2211]), svmhsi[,3][1966:2211])
test1_svmhsi = data.frame(svmhsi[,1][2212:2457], log(svmhsi[,2][2212:2457]), svmhsi[,3][2212:2457])

col.headings = c('Date', 'close', 'Days')
names(train70_svmhsi) = col.headings
names(train80_svmhsi) = col.headings
names(train90_svmhsi) = col.headings
names(test3_svmhsi) = col.headings
names(test2_svmhsi) = col.headings
names(test1_svmhsi) = col.headings
col.headingss = c('Date', 'close', 'Days', 'Return')
names(svmhsi) = col.headings

#----------------------------- fitting SVM for 3 yrs-------------------------------------#
regressor_rad3 = svm(formula = close ~ Days, data = train70_svmhsi, type = 'eps-regression', kernel = 'radial', cost = 1)
regressor_poly3 = svm(formula = close ~ Days, data = train70_svmhsi, type = 'eps-regression', kernel = 'polynomial', cost = 1, degree = 3)
regressor_lin3 = svm(formula = close ~ Days, data = train70_svmhsi, type = 'eps-regression', kernel = 'linear', cost = 1)
summary(regressor_lin3)
summary(regressor_poly3)
summary(regressor_rad3)

regressor_rad2 = svm(formula = close ~ Days, data = train80_svmhsi, type = 'eps-regression', kernel = 'radial', cost = 25, epsilon = 0.2)
regressor_poly2 = svm(formula = close ~ Days, data = train80_svmhsi, type = 'eps-regression', kernel = 'polynomial', cost = 25, degree = 3)
regressor_lin2 = svm(formula = close ~ Days, data = train80_svmhsi, type = 'eps-regression', kernel = 'linear', cost = 25)
summary(regressor_lin2)
summary(regressor_poly2)
summary(regressor_rad2)

regressor_rad1 = svm(formula = close ~ Days, data = train90_svmhsi, type = 'eps-regression', kernel = 'radial', cost = 0.1)
regressor_poly1 = svm(formula = close ~ Days, data = train90_svmhsi, type = 'eps-regression', kernel = 'polynomial', cost = 0.1, degree = 3)
regressor_lin1 = svm(formula = close ~ Days, data = train90_svmhsi, type = 'eps-regression', kernel = 'linear', cost = 0.1)
summary(regressor_lin1)
summary(regressor_poly1)
summary(regressor_rad1)

#-----------------------------Prediction of SVM for 3 yrs-------------------------------------#
predictor_lin3 = predict(regressor_lin3, test3_svmhsi)
predictor_poly3 = predict(regressor_poly3,test3_svmhsi)
predictor_rad3 = predict(regressor_rad3,test3_svmhsi)
table_lin3 = data.frame(exp(test3_svmhsi$close), exp(predictor_lin3), test3_svmhsi$Days)
table_poly3 = data.frame(exp(test3_svmhsi$close), exp(predictor_poly3), test3_svmhsi$Days)
table_rad3 = data.frame(exp(test3_svmhsi$close), exp(predictor_rad3), test3_svmhsi$Days)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(table_lin3) = col.headings
names(table_poly3) = col.headings
names(table_rad3) = col.headings

#-----------------------------Prediction of SVM for 2 yrs-------------------------------------#
predictor_lin2 = predict(regressor_lin2, test2_svmhsi)
predictor_poly2 = predict(regressor_poly2, test2_svmhsi)
predictor_rad2 = predict(regressor_rad2, test2_svmhsi)
table_lin2 = data.frame(exp(test2_svmhsi$close), exp(predictor_lin2), test2_svmhsi$Days)
table_poly2 = data.frame(exp(test2_svmhsi$close), exp(predictor_poly2), test2_svmhsi$Days)
table_rad2 = data.frame(exp(test2_svmhsi$close), exp(predictor_rad2), test2_svmhsi$Days)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(table_lin2) = col.headings
names(table_poly2) = col.headings
names(table_rad2) = col.headings

#-----------------------------Prediction of SVM for 1 yrs-------------------------------------#
predictor_lin1 = predict(regressor_lin1, test1_svmhsi)
predictor_poly1 = predict(regressor_poly1, test1_svmhsi)
predictor_rad1 = predict(regressor_rad1, test1_svmhsi)
table_lin1 = data.frame(exp(test1_svmhsi$close), exp(predictor_lin1), test1_svmhsi$Days)
table_poly1 = data.frame(exp(test1_svmhsi$close), exp(predictor_poly1), test1_svmhsi$Days)
table_rad1 = data.frame(exp(test1_svmhsi$close), exp(predictor_rad1), test1_svmhsi$Days)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(table_lin1) = col.headings
names(table_poly1) = col.headings
names(table_rad1) = col.headings

#-----------------------------Visualiazation of prediction for 3 yrs-------------------------------------#
ggplot() + 
  geom_line(aes(x = svmhsi$Days[1:1719], y = svmhsi$close[1:1719]), colour = 'red') +
  geom_line(aes(x = svmhsi$Days[1720:2457], y = svmhsi$close[1720:2457]), colour = 'orange') +
  geom_line(aes(x = table_lin3$Days, y = table_lin3$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = table_poly3$Days, y = table_poly3$`Forecasted Price`), colour = 'yellow') +
  geom_line(aes(x = table_rad3$Days, y = table_rad3$`Forecasted Price`), colour = 'green') +
  ggtitle('Prediction of HSI for 3 years') +
  xlab('Days') +
  ylab('HSI closing price')

#-----------------------------Visualiazation of prediction for 2 yrs-------------------------------------#
ggplot() + 
  geom_line(aes(x = svmhsi$Days[1:1965], y = svmhsi$close[1:1965]), colour = 'red') +
  geom_line(aes(x = svmhsi$Days[1966:2211], y = svmhsi$close[1966:2211]), colour = 'orange') +
  geom_line(aes(x = table_lin2$Days, y = table_lin2$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = table_poly2$Days, y = table_poly2$`Forecasted Price`), colour = 'yellow') +
  geom_line(aes(x = table_rad2$Days, y = table_rad2$`Forecasted Price`), colour = 'green') +
  ggtitle('Prediction of HSI for 2 years') +
  xlab('Days') +
  ylab('HSI closing price')

#-----------------------------Visualiazation of prediction for 1 yrs-------------------------------------#
ggplot() + 
  geom_line(aes(x = svmhsi$Days[1:2211], y = svmhsi$close[1:2211]), colour = 'red') +
  geom_line(aes(x = svmhsi$Days[2212:2457], y = svmhsi$close[2212:2457]), colour = 'orange') +
  geom_line(aes(x = table_lin1$Days, y = table_lin1$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = table_poly1$Days, y = table_poly1$`Forecasted Price`), colour = 'yellow') +
  geom_line(aes(x = table_rad1$Days, y = table_rad1$`Forecasted Price`), colour = 'green') +
  ggtitle('Prediction of HSI for 1 years') +
  xlab('Days') +
  ylab('HSI closing price')

#-----------------------------Visualiazation of prediction for 2 yrs-------------------------------------#
ggplot() + 
  geom_line(aes(x = svmhsi$Days[1:1965], y = svmhsi$close[1:1965]), colour = 'red') +
  geom_line(aes(x = svmhsi$Days[1966:2457], y = svmhsi$close[1966:2457]), colour = 'orange') +
  geom_line(aes(x = table_rad2$Days, y = table_rad2$`Forecasted Price`), colour = 'green') +
  geom_line(aes(x = df1$Days, y = df1$`Forecasted Price`), colour = 'blue') +
  ggtitle('Prediction of HSI for 2 years') +
  xlab('Days') +
  ylab('HSI closing price')



#------------------------------ (MAPE) for 3yrs -------------------------------#
MAPE_rad3 = mean(abs((table_rad3$`Actual Price`-table_rad3$`Forecasted Price`)/table_rad3$`Actual Price`)) 
MAPE_poly3 = mean(abs((table_poly3$`Actual Price`-table_poly3$`Forecasted Price`)/table_poly3$`Actual Price`))
MAPE_lin3 = mean(abs((table_lin3$`Actual Price`-table_lin3$`Forecasted Price`)/table_lin3$`Actual Price`))
MAPE_rad3 
MAPE_poly3
MAPE_lin3

#------------------------------ Finding RMSE for 3yrs -------------------------------#
RMSE_rad3 = RMSE(table_rad3$`Actual Price`, table_rad3$`Forecasted Price`) 
RMSE_poly3 = RMSE(table_poly3$`Actual Price`, table_poly3$`Forecasted Price`) 
RMSE_lin3 = RMSE(table_lin3$`Actual Price`, table_lin3$`Forecasted Price`) 
RMSE_rad3
RMSE_poly3
RMSE_lin3

#------------------------------ Finding MAE for 3yrs -------------------------------#
MAE_rad3 = MAE(table_rad3$`Actual Price`, table_rad3$`Forecasted Price`)
MAE_poly3 = MAE(table_poly3$`Actual Price`, table_poly3$`Forecasted Price`)
MAE_lin3 = MAE(table_lin3$`Actual Price`, table_lin3$`Forecasted Price`)
MAE_rad3
MAE_poly3
MAE_lin3

#------------------------------ (MAPE) for 2yrs -------------------------------#
MAPE_rad2 = mean(abs((table_rad2$`Actual Price`-table_rad2$`Forecasted Price`)/table_rad2$`Actual Price`)) 
MAPE_poly2 = mean(abs((table_poly2$`Actual Price`-table_poly2$`Forecasted Price`)/table_poly2$`Actual Price`)) 
MAPE_lin2 = mean(abs((table_lin2$`Actual Price`-table_lin2$`Forecasted Price`)/table_lin2$`Actual Price`)) 
MAPE_rad2
MAPE_poly2
MAPE_lin2

#------------------------------ Finding RMSE for 2yrs -------------------------------#
RMSE_rad2 = RMSE(table_rad2$`Actual Price`, table_rad2$`Forecasted Price`)
RMSE_poly2 = RMSE(table_poly2$`Actual Price`, table_poly2$`Forecasted Price`) 
RMSE_lin2 = RMSE(table_lin2$`Actual Price`, table_lin2$`Forecasted Price`) 
RMSE_rad2
RMSE_poly2
RMSE_lin2

#------------------------------ Finding MAE for 2yrs -------------------------------#
MAE_rad2 = MAE(table_rad2$`Actual Price`, table_rad2$`Forecasted Price`)
MAE_poly2 = MAE(table_poly2$`Actual Price`, table_poly2$`Forecasted Price`)
MAE_lin2 = MAE(table_lin2$`Actual Price`, table_lin2$`Forecasted Price`)
MAE_rad2
MAE_poly2
MAE_lin2

#------------------------------ (MAPE) for 1yrs -------------------------------#
MAPE_rad1 = mean(abs((table_rad1$`Actual Price`-table_rad1$`Forecasted Price`)/table_rad1$`Actual Price`)) 
MAPE_poly1 = mean(abs((table_poly1$`Actual Price`-table_poly1$`Forecasted Price`)/table_poly1$`Actual Price`)) 
MAPE_lin1 = mean(abs((table_lin1$`Actual Price`-table_lin1$`Forecasted Price`)/table_lin1$`Actual Price`)) 
MAPE_rad1
MAPE_poly1
MAPE_lin1

#------------------------------ Finding RMSE for 1yrs -------------------------------#
RMSE_rad1 = RMSE(log(table_rad1$`Actual Price`), log(table_rad1$`Forecasted Price`))
RMSE_poly1 = RMSE(log(table_poly1$`Actual Price`), log(table_poly1$`Forecasted Price`)) 
RMSE_lin1 = RMSE(log(table_lin1$`Actual Price`), log(table_lin1$`Forecasted Price`)) 
RMSE_rad1
RMSE_poly1
RMSE_lin1

#------------------------------ Finding MAE for 1yrs -------------------------------#
MAE_rad1 = MAE(table_rad1$`Actual Price`, table_rad1$`Forecasted Price`)
MAE_poly1 = MAE(table_poly1$`Actual Price`, table_poly1$`Forecasted Price`)
MAE_lin1 = MAE(table_lin1$`Actual Price`, table_lin1$`Forecasted Price`)
MAE_rad1
MAE_poly1
MAE_lin1







#-----------------------------CV for SVM-------------------------------------#
control = trainControl(method = 'repeatedcv', number = 20, repeats = 3)
model = train(close ~ Days, data = train_svmhsi, method = 'svmRadialCost', trControl = control)
print(model)
plot(model) #cost = 1

control2 = trainControl(method = 'repeatedcv', number = 20, repeats = 3)
model2 = train(close ~ Days, data = train_svmhsi, method = 'svmRadialSigma', trControl = control2)
print(model2)
plot(model2) #The final values used for the model were sigma = 33.99872 and C = 1.

control4 = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model4 = train(close ~ Days, data = train_svmhsi, method = 'svmPoly', trControl = control4)
print(model4)
plot(model4) #The final values used for the model were degree = 3, scale = 0.1 and C = 1

#----------------------------- Corss-Validation -------------------------------------#
#Linear
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="linear" ,ranges =list(cost=c(0.1 ,1 , 5, 15, 30, 50)))
summary(tune.out) 
bestmod = tune.out$best.model
summary(bestmod)
#C = 5 for 60% train 40% test; 2016-2020 (4yrs)
#C = 1 for 70% train 30% test; 2017-2020 (3yrs)
#C =   for 80% train 20% test; 2018-2020 (2yrs)

#Polynomial
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="poly" ,ranges =list(cost=c(0.1 ,1 , 5, 15, 30, 50)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
#C = 0.001 & Degree = 3 for 60% train 40% test; 2016-2020 (4yrs)
#C = 5 & Degree = 3 for 70% train 30% test; 2017-2020 (3yrs)
#C =    for 80% train 20% test; 2018-2020 (2yrs)

#Gausian Radial
tune.out=tune(svm ,close ~ Days,data=train_svmhsi, kernel ="radial" ,ranges =list(epsilon = seq(0,1,0.1), cost = 1:100))
print(tune.out)
plot(tune.out)
#summary(tune.out) 
bestmod = tune.out$best.model
#summary(bestmod) #C = 50

#-----------------------------------------Questions-------------------------------------------------#
#What's the 'number' and 'repeats' meaning in CV???
#What's the 'sigma' meaning in CV???
#Waht's the 'degree' and 'scale' meaning???
#What's trainControl???
#Polynomial and linear SVR???
#Any other value that can test model accuracy???