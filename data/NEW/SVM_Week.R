#--------------------------------Loading Data----------------------------------------#
fileName = "Closeweek.csv"
defaultDataDir = "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/"
fileLocation = file.path(defaultDataDir, fileName)
hsiww = read.csv(fileLocation, header = T)
dim(hsiww)

#-----------------------------Loading Libraries-------------------------------------#
library(tidyverse)
library(e1071)
library(ggplot2)

library(caret)
library(kernlab)
library(lattice)

#-----------------------------define train and test data for 70%-------------------------------------#
train_hsiww = data.frame(scale(hsiww[,2][1:706]), hsiww[,1][1:706], hsiww[,7][1:706], scale(hsiww[,3][1:706]), scale(hsiww[,4][1:706]), scale(hsiww[,5][1:706]), scale(hsiww[,6][1:706]), scale(hsiww[,8][1:706]))
col.headings = c('Close', 'date', 'Weeks', 'SMA', 'RSI', 'ROC', 'CCI', 'MTM')
names(train_hsiww) = col.headings

train_hsiww2 = data.frame(hsiww[,2][1:706], hsiww[,1][1:706], hsiww[,7][1:706], scale(hsiww[,3][1:706]), scale(hsiww[,4][1:706]), scale(hsiww[,5][1:706]), scale(hsiww[,6][1:706]), scale(hsiww[,8][1:706]))
col.headings = c('Close', 'date', 'Weeks', 'SMA', 'RSI', 'ROC', 'CCI', 'MTM')
names(train_hsiww2) = col.headings
ggplot() + 
  geom_line(aes(x = train_hsiww2$Weeks, y = train_hsiww2$Close), colour = 'black') +
  ggtitle('The time series of closing price in Hang Seng Index') +
  xlab('Weeks') +
  ylab('Closing price')

test_hsiww = data.frame(scale(hsiww[,2][707:758]), hsiww[,1][707:758], hsiww[,7][707:758], scale(hsiww[,3][707:758]), scale(hsiww[,4][707:758]), scale(hsiww[,5][707:758]), scale(hsiww[,6][707:758]), scale(hsiww[,8][707:758]))
names(test_hsiww) = col.headings
#----------------------------- fitting SVM-------------------------------------#
regressor_rad = svm(formula = Close ~ SMA + MTM + CCI + RSI, data = train_hsiww, type = 'eps-regression', kernel = 'radial', cost = 45, sigma = 0.01)
regressor_poly = svm(formula = Close ~ SMA + MTM + CCI + RSI, data = train_hsiww, type = 'eps-regression', kernel = 'polynomial', cost = 1, degree = 3)
regressor_lin = svm(formula = Close ~ SMA + MTM + CCI + RSI, data = train_hsiww, type = 'eps-regression', kernel = 'linear', cost = 0.25)
par(mfrow = c(1,1))
plot(train_hsiww$Weeks, train_hsiww$Close, type = 'l', lwd = '1', col = 'orange', xlab = "Weeks", ylab = "Scaled Closing Price of HSI")
lines(fitted(regressor_rad), col = 3)
lines(fitted(regressor_lin), col = 4)
lines(fitted(regressor_poly), col = 'grey')
legend("topleft", legend = c("Actual", "Fitting of Linear SVR", "Fitting of Polynomial SVR", "Fitting of Radial SVR"), col = c('Orange',4,"grey",3), lty = c(1,1,1,1), lwd = 2, bty = 'n',text.width =40000, seg.len = 0.8)
title("Fitting of three SVRs")

#-----------------------------Prediction of SVM-------------------------------------#
predictor_lin = predict(regressor_lin, test_hsiww)
predictor_poly = predict(regressor_poly,test_hsiww)
predictor_rad = predict(regressor_rad,test_hsiww)

lin = predictor_lin * sd(hsiww[,2][707:758]) + mean(hsiww[,2][707:758]) #unscale
poly = predictor_poly * sd(hsiww[,2][707:758]) + mean(hsiww[,2][707:758]) #unscale
rad = predictor_rad * sd(hsiww[,2][707:758]) + mean(hsiww[,2][707:758]) #unscale
table_lin = data.frame(hsiww[,2][707:758], lin, test_hsiww$Weeks)
table_poly = data.frame(hsiww[,2][707:758], poly, test_hsiww$Weeks)
table_rad = data.frame(hsiww[,2][707:758], rad, test_hsiww$Weeks)
col.headings = c('Actual Price', 'Forecasted Price', 'Weeks')
names(table_lin) = col.headings
names(table_poly) = col.headings
names(table_rad) = col.headings

#-----------------------------Visualiazation-------------------------------------#
ggplot() + 
  #geom_line(aes(x = hsidd$Date2[1:241], y = hsidd$Close[1:241]), colour = 'red') +
  geom_line(aes(x = table_lin$Weeks, y = table_lin$`Actual Price`), colour = 'orange') +
  geom_line(aes(x = table_lin$Weeks, y = table_lin$`Forecasted Price`), colour = 'blue') +
  #geom_line(aes(x = table_poly$Weeks, y = table_poly$`Forecasted Price`), colour = 'grey') +
  geom_line(aes(x = table_rad$Weeks, y = table_rad$`Forecasted Price`), colour = 'green') +
  ggtitle('Prediction for 319 weeks') +
  xlab('Weeks') +
  ylab('HSI closing price')

par(mfrow = c(1,1))
plot(table_lin$Weeks, table_lin$`Actual Price`, col = 'black', xlab = "Weeks", ylab = 'Closing Price of HSI', lwd = 1)
lines(table_lin$Weeks, table_lin$`Forecasted Price`, col = 10, lwd = 1.5)
#lines(table_poly$Weeks, table_poly$`Forecasted Price`, col = 'grey', lwd = 1)
lines(table_rad$Weeks, table_rad$`Forecasted Price`, col = 4, lwd = 1.5)
legend("topleft", legend = c("Forecast from Linear SVR", "Forecast from Radial SVR"), col = c(10,4), lty = c(1,1), lwd = 2, bty = 'n',text.width =40000, seg.len = 0.8)
title("Prediction for 52 weeks")
#------------------------------ MAPE -------------------------------#

mean(abs((table_rad$`Actual Price`- table_rad$`Forecasted Price`)/table_rad$`Actual Price`)) 
mean(abs((table_poly$`Actual Price`- table_poly$`Forecasted Price`)/table_poly$`Actual Price`))
mean(abs((table_lin$`Actual Price`- table_lin$`Forecasted Price`)/table_lin$`Actual Price`))

#------------------------------ Finding RMSE -------------------------------#
RMSE(table_rad$`Actual Price`, table_rad$`Forecasted Price`) 
RMSE(table_poly$`Actual Price`, table_poly$`Forecasted Price`) 
RMSE(table_lin$`Actual Price`, table_lin$`Forecasted Price`) 

#-----------------------------CV-------------------------------------#
svrGrid <- expand.grid(sigma = (0.01:30), C = (0.01:30)*5)
svrGrid1 <- expand.grid(degree = c(2,3,4), scale = (0.1:10), C = (0.01:30))
svrGrid2 <- expand.grid(cost = (0.01:30)*5)

control = trainControl(method = 'repeatedcv', number = 10, repeats = 5)
model1 = train(Close ~ SMA + RSI + MTM + CCI, data = train_hsiww, method = 'svmRadial', trControl = control, tuneGrid = svrGrid)
print(model1)
plot(model1) 
?train
?trainControl

model2 = train(Close ~ SMA + RSI + MTM + CCI, data = train_hsiww, method = 'svmPoly', trControl = control, tuneGrid = svrGrid1)
print(model2)
plot(model2) 

model3 = train(Close ~ SMA + RSI + MTM + CCI, data = train_hsiww, method = 'svmLinear2', trControl = control, tuneGrid = svrGrid2)
print(model3)
plot(model3) 
