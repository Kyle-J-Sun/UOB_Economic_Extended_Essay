# Script Name: ARIMA for HSIM.R
# Author: Jingkai Sun (ks3020@ic.ac.uk)

# predicting monthly HSI data using ARIMA

#--------------------------------Loading Data----------------------------------------#
fileName = "HSIMR.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
fileLocation = file.path(defaultDataDir, fileName)
hsir = read.csv(fileLocation, header = T)
dim(hsir)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)
library(dplyr)

#-----------------------------Plot and convert to ln format-------------------------------------#
train_hsir = log(hsir[,2][1:167])#build training data
test_hsir = log(hsir[,2][168:143])
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_hsir) #we get q value from acf       
# We have descent in our lags
pacf(train_hsir) #we get p value from pacf     p = 1 here
# Partial Autocorrelation Function
#First lag: Big Drop
print(adf.test(train_hsir)) #p = 0.1771 > 0.05
#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_hsir = diff(train_hsir, 1) # d = 1
adf.test(difftrain_hsir)# p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
hsir_arima = ts(train_hsir, start = 1, frequency = 12)
# AUTO Arima
fittrain_hsir = auto.arima(hsir_arima)
tsdisplay(residuals(fittrain_hsir), lag.max = 40) #ARIMA(2,1,2) ; AIC/BIC = -10339/-10306 = 1
par(mfrow = c(1,1))
plot(hsir_arima, type = 'l')
lines(fitted(fittrain_hsir), col = 'red')
title('hsi Price')

# Custom Arima
fitA = arima(train_hsir, order = c(1,1,8))
tsdisplay(residuals(fitA), lag.max = 40)

fitB = arima(train_hsir, order = c(2,1,8))
tsdisplay(residuals(fitB), lag.max = 40)

#----------------------------------Runing Data using Auto ARIMA----------------------------------#
par(mfrow = c(2,2))
term = 67
fcast1 = forecast(fittrain_hsir, h = term)
plot(fcast1)
fcast1_ext = as.numeric(fcast1$mean)
final_facast1 = exp(fcast1_ext)

fcast2 = forecast(fitA, h = term)
plot(fcast2)
fcast2_ext = as.numeric(fcast2$mean)
final_facast2 = exp(fcast2_ext)

fcast3 = forecast(fitB, h = term)
plot(fcast3)
fcast3_ext = as.numeric(fcast3$mean)
final_facast3 = exp(fcast3_ext)

#------------------------------ Finding Mean Percentage Error(MPE) -------------------------------#
df1 = data.frame(hsir[,2][177:243], final_facast1, hsir[,3][177:243])
col.headings = c('Actual Price', 'Forecasted Price', 't')
names(df1) = col.headings
attach(df1)
percentage.error1 = ((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`))
percentage.error1
mean(percentage.error1) #0.5% deviation on average.

df2 = data.frame(hsir[,2][177:243], final_facast2, hsir[,3][177:243])
col.headings = c('Actual Price', 'Forecasted Price', 't')
names(df2) = col.headings
attach(df2)
percentage.error2 = ((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`))
percentage.error2
mean(percentage.error2) 
#9% for (1,2,11)

df3 = data.frame(hsir[,2][177:243], final_facast3, hsir[,3][177:243])
col.headings = c('Actual Price', 'Forecasted Price', 't')
names(df3) = col.headings
attach(df3)
percentage.error3 = ((df3$`Actual Price`-df3$`Forecasted Price`)/(df3$`Actual Price`))
percentage.error3
mean(percentage.error3) 
#8% for (2,2,11)
write.csv(df1, '/Users/kyle/Documents/Economics Extended Essay/Data/HSI/df1m.csv')

#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  geom_line(aes(x = df1$t, y = df1$`Actual Price`), colour = 'red') +
  geom_line(aes(x = df1$t, y = df1$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = df2$t, y = df2$`Forecasted Price`), colour = 'yellow') +
  geom_line(aes(x = df3$t, y = df3$`Forecasted Price`), colour = 'green') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')

#------------------------------ Finding RMSE -------------------------------#
accuracy(fcast1) #0.064
accuracy(fcast2) #0.062
accuracy(fcast3) #0.061

#------------------------------ Finding MAPE -------------------------------#
accuracy(fcast1) #0.499% 
accuracy(fcast2) #0.491%
accuracy(fcast3) #0.478%

#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
#Test whether there is sufficient structure in a time series to make it worth modelling
#Applied to residuals after fit to test whether there is remaining structure in the residuals
#Small P-values indicate significant autocorrelation

Box.test(fittrain_hsir$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_hsir$residuals, lag = 25, type = 'Ljung-Box')
Box.test(fittrain_hsir$residuals, lag = 40, type = 'Ljung-Box')
#p-value = 0.98, 0.17 and 0.05 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

Box.test(fitA$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 25, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 40, type = 'Ljung-Box')
#p-value = 1, 0.56 and 0.28 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

Box.test(fitB$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitB$residuals, lag = 25, type = 'Ljung-Box')
Box.test(fitB$residuals, lag = 40, type = 'Ljung-Box')
#p-value = 1, 1 and 0.94 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

#-----------------------------------------Questions-------------------------------------------------#
#Why MPE calculated by self differs from the MPE in accuracy function???
#Any other methods to evaluate model???
