rm(list = ls())
#--------------------------------Loading Data----------------------------------------#
fileName = "Closeweek.csv"
defaultDataDir = "/Users/kyle/Documents/EEE/Data/"
fileLocation = file.path(defaultDataDir, fileName)
hsiweek = read.csv(fileLocation, header = T)
dim(hsiweek)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
library(tseries)
library(forecast)
library(dplyr)
library(lmtest)
library(orcutt)
library(egcm)
library(ggplot2)
library(tidyverse)
library(e1071)


#-----------------------------Plot and convert to ln format-------------------------------------#
train_hsiweek = log(hsiweek[,2][1:706])
adf.test(train_hsiweek)
par(mfrow = c(1,2))
acf(train_hsiweek, main = 'ACF for the original training data')      
pacf(train_hsiweek, main = 'PACF for the original training data')

#-----------------------------ACF, PACF and Dickey-Fuller Test for 3 yrs-------------------------------------#
difftrain_hsiweek = diff(train_hsiweek, 1) # d = 1
second_difftrain_hsi = diff(train_hsiweek, 1, 2)

#------------Get first order difference acf and pacf 
adf.test(difftrain_hsiweek)
par(mfrow = c(2,2))
acf(difftrain_hsiweek, main = 'ACF for the first-order differencing data')      
pacf(difftrain_hsiweek, main = 'PACF for the first-order differencing data')
acf(second_difftrain_hsi, main = 'ACF test for second-order differencing data')     
pacf(second_difftrain_hsi, main = 'PACF test for second-order differencing data')
#These diagrams tell us choosing ARIMA (14,1,1) or ARIMA (14,1,14)

#------------Get second order difference acf and pacf 
adf.test(second_difftrain_hsi)
plot(second_difftrain_hsi, type = 'l')
par(mfrow = c(1,2))

#These diagrams tell us choosing ARIMA (5,2,2) (7,2,2)

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_hsi = diff(train_hsiweek, 1) # d = 1
adf.test(difftrain_hsi)# p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

par(mfrow = c(1,2))
acf(difftrain_hsi, main = 'ACF test for training data')      
pacf(difftrain_hsi, main = 'PACF test for training data') 

#------------------------------Running Data using Auto ARIMA-------------------------------#
tsarima = ts(train_hsiweek, start = c(1,1), frequency = 1)
fitA = arima(tsarima, order = c(0,1,0), optim.control = list(maxit = 1000))
tsdisplay(residuals(fitA), lag.max = 40, main = 'Residuals (0,1,0)')
BIC(fitA)
summary(fitA)
#AIC = -2781.12 BIC = -2735.549

fitB = arima(tsarima, order = c(0,1,1), optim.control = list(maxit = 1000), method = "ML")
tsdisplay(residuals(fitB), lag.max = 40, main = 'Residuals (0,1,1)')
BIC(fitB)
# accuracy(fitB)
summary(fitB)
par(mfrow = c(1,1))
plot(tsarima, xlab = 'Weeks', ylab = "HSI Index Price (Log)", main = 'The fit of ARIMA (0,1,1)')
lines(fitted(fitB), col = 'red')
#AIC = -2783.41 BIC = -2751.513

fitC = arima(tsarima, order = c(1,1,1), optim.control = list(maxit = 1000))
tsdisplay(residuals(fitC), lag.max = 40, main = 'Residuals (1,1,1)')
BIC(fitC)
summary(fitC)
#AIC = -2778.59 BIC = -2710.242

#----------------------------------Runing Data using Auto ARIMA----------------------------------#
par(mfrow = c(1,2))
term = 319

par(mfrow = c(1,1))
fcast1 = forecast(fitA, h = term)
plot(fcast1, xlab = 'Weeks', ylab = 'Logarithm of HSI closing price')
fcast1_ext = as.numeric(fcast1$mean)
final_facast1 = exp(fcast1_ext)

par(mfrow = c(1,3))
fcast21 = forecast(fitB, h = 319)
plot(fcast21, xlab = 'Weeks', ylab = 'Logarithmic losing price in HSI', main = "Frecasts from ARIMA (5,2,1) for 319 weeks")
fcast22 = forecast(fitB, h = 104)
plot(fcast22, xlab = 'Weeks', ylab = 'Logarithmic losing price in HSI', main = "Frecasts from ARIMA (5,2,1) for 104 weeks")
fcast23 = forecast(fitB, h = 52)
plot(fcast23, xlab = 'Weeks', ylab = 'Logarithmic losing price in HSI', main = "Frecasts from ARIMA (5,2,1) for 52 weeks")
fcast21_ext = as.numeric(fcast21$mean)
final_facast21 = exp(fcast21_ext)
fcast22_ext = as.numeric(fcast22$mean)
final_facast22 = exp(fcast22_ext)
fcast23_ext = as.numeric(fcast23$mean)
final_facast23 = exp(fcast23_ext)

par(mfrow = c(1,1))
fcast3 = forecast(fitC, h = term)
plot(fcast3, xlab = 'Weeks', ylab = 'Logarithm of HSI closing price')
fcast3_ext = as.numeric(fcast3$mean)
final_facast3 = exp(fcast3_ext)
#------------------------------ Crate dataframe for actual and predicted prices -------------------------------#
df1 = data.frame(hsiweek[,2][707:1025], final_facast1, hsiweek[,7][707:1025])
col.headings = c('Actual Price', 'Forecasted Price', 'Weeks')
names(df1) = col.headings

df21 = data.frame(hsiweek[,2][707:1025], final_facast21, hsiweek[,7][707:1025])
col.headings = c('Actual Price', 'Forecasted Price', "Weeks")
names(df21) = col.headings
df22 = data.frame(hsiweek[,2][707:810], final_facast22, hsiweek[,7][707:810])
col.headings = c('Actual Price', 'Forecasted Price', "Weeks")
names(df22) = col.headings
df23 = data.frame(hsiweek[,2][707:758], final_facast23, hsiweek[,7][707:758])
col.headings = c('Actual Price', 'Forecasted Price', "Weeks")
names(df23) = col.headings


df3 = data.frame(hsiweek[,2][707:1025], final_facast2, hsiweek[,7][707:1025])
col.headings = c('Actual Price', 'Forecasted Price', "Weeks")
names(df3) = col.headings

#------------------------------ Finding Mean Absolute Percentage Error(MAPE) -------------------------------#
mean(abs((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`))) #0.0807 -- 8.07%

#------------------------------ Finding RMSE -------------------------------#
RMSE(df2$`Actual Price`, df2$`Forecasted Price`)
accuracy(fcast2)

#------------------------------ Finding MAE for 2yrs -------------------------------#
MAE(df2$`Actual Price`, df2$`Forecasted Price`)


#---------------------------------Ljung-Box-----------------------------------#
Box.test(fitA$residuals, lag = 15, type = 'Ljung-Box')
par(mfrow = c(1,2))
acf(fitA$residuals, main = 'ACF of residuals')    
pacf(fitA$residuals, main = 'PACF of residuals')

Box.test(fitB$residuals, lag = 18, type = 'Ljung')
par(mfrow = c(1,2))
acf(fitB$residuals, main = 'ACF of residuals')    
pacf(fitB$residuals, main = 'PACF of residuals')

Box.test(fitC$residuals, lag = 30, type = 'Ljung-Box')
par(mfrow = c(1,2))
acf(fitC$residuals, main = 'ACF of residuals')    
pacf(fitC$residuals, main = 'PACF of residuals')

#-----------------------------Visualiazation of prediction-------------------------------------#
par(mfrow = c(1,3))
plot(df21$Weeks, df21$`Actual Price`, col = 1, xlab = "Weeks", ylab = 'Closing Price of HSI', main = "Forecasts from ARIMA(5,2,1) for 319 weeks")
lines(df21$Weeks, df21$`Forecasted Price`, col = 2, lwd = 2)
plot(df22$Weeks, df22$`Actual Price`, col = 1, xlab = "Weeks", ylab = 'Closing Price of HSI', main = "Forecasts from ARIMA(5,2,1) for 104 weeks")
lines(df22$Weeks, df22$`Forecasted Price`, col = 2, lwd = 2)
plot(df23$Weeks, df23$`Actual Price`, col = 1, xlab = "Weeks", ylab = 'Closing Price of HSI', main = "Forecasts from ARIMA(5,2,1) for 52 weeks")
lines(df23$Weeks, df23$`Forecasted Price`, col = 2, lwd = 2)

plot(df2$Weeks, df2$`Actual Price`, col = 2)
legend(x = 700,y = 34000, legend = c("Actual", "Forecast of ARIMA (1,1,0)", "Forecast of ARIMA (14,2,14)"), col = c('Orange',10,3), lty = c(1,1,1), lwd = 2, bty = 'n', text.width = 0, seg.len = 1)
title("Predicted Price of HSI using ARIMA")

ggplot() + 
  geom_line(aes(x = df2$Weeks, y = df2$`Actual Price`), colour = 'orange') +
  geom_line(aes(x = df2$Weeks, y = df2$`Forecasted Price`), colour = 'blue') +
  ggtitle('Prediction for 319 weeks') +
  xlab('Weeks') +
  ylab('HSI closing price')



#------------------------Accuracy of Prediction------------------------#
Acc = data.frame(hsiweek[,7][707:1025], hsiweek[,2][707:1025], df2$`Forecasted Price`, table_lin[,2], table_rad[,2])
col.headings = c('Weeks', 'ap', 'fp_arima_522','fp_lin', 'fp_rad')
names(Acc) = col.headings
write.csv(Acc, file = "/Users/kyle/Documents/EEE/result/Accuracy2.csv")
