# Script Name: ARIMA HSI D.R
# Author: Jingkai Sun (ks3020@ic.ac.uk)

# predicting daily HSI dataset using ARIMA model

#--------------------------------Loading Data----------------------------------------#
fileName = "HSID.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
fileLocation = file.path(defaultDataDir, fileName)
hsi = read.csv(fileLocation, header = T)
dim(hsi)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)
library(dplyr)
library(lmtest)
library(orcutt)
library(egcm)

#-----------------------------Plot and convert to ln format-------------------------------------#
train70_hsi = log(hsi[,2][1:1719])
train80_hsi = log(hsi[,2][1:1965])
train90_hsi = log(hsi[,2][1:2211])
#train_hsi = hsi[,2][1:1965]
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test for 3 yrs-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train70_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(train70_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train70_hsi)) 
#p = 0.1771 > 0.05
#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#-----------------------------ACF, PACF and Dickey-Fuller Test for 2 yrs-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train80_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(train80_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train80_hsi)) 
#p = 0.1771 > 0.05
#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#-----------------------------ACF, PACF and Dickey-Fuller Test for 1 yr-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train90_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(train90_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train90_hsi)) 
#p = 0.1771 > 0.05
#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.



#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain70_hsi = diff(train70_hsi, 1) # d = 1
adf.test(difftrain70_hsi)# p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

difftrain80_hsi = diff(train80_hsi, 1)
adf.test(difftrain80_hsi)

difftrain90_hsi = diff(train90_hsi, 1)
adf.test(difftrain90_hsi)
plot(difftrain80_hsi)

par(mfrow = c(1,2))
acf(difftrain80_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(difftrain80_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train90_hsi)) 

#------------------------------Runing Data using Auto ARIMA-------------------------------#
hsi70_arima = ts(train70_hsi, start = 1, frequency = 1)
hsi80_arima = ts(train80_hsi, start = 1, frequency = 1)
hsi90_arima = ts(train90_hsi, start = 1, frequency = 1)
# AUTO Arima
fittrain70_hsi = auto.arima(hsi70_arima) # AIC=-11992.58   AICc=-11992.58   BIC=-11987
fittrain80_hsi = auto.arima(hsi80_arima)
fittrain90_hsi = auto.arima(hsi90_arima)

#tsdisplay(residuals(fittrain_hsi), lag.max = 40, main = 'Residuals') #ARIMA(0,1,0) ; AIC/BIC = 11992/11987 = 1
par(mfrow = c(2,2))

plot(hsi70_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of auto ARIMA(0,1,0)')
lines(fitted(fittrain70_hsi), col = 'red')

plot(hsi80_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of auto ARIMA(0,1,0)')
lines(fitted(fittrain80_hsi), col = 'red')

plot(hsi90_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of auto ARIMA(0,1,0)')
lines(fitted(fittrain90_hsi), col = 'red')

# Custom Arima
fitA70 = arima(train70_hsi, order = c(1,1,1)) #AIC = -11989.5   BIC = -11972.76     11990/11973 = 1
tsdisplay(residuals(fitA70), lag.max = 40, main = 'Residuals (1,1,1)')
plot(hsi70_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(1,1,1)')
lines(fitted(fitA70), col = 'blue')

fitA80 = arima(train80_hsi, order = c(1,1,0)) #AIC = -11991.5 -11980.33 = 
BIC(fitA80)
tsdisplay(residuals(fitA80), lag.max = 40, main = 'Residuals (1,1,1)')
plot(hsi80_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(1,1,1)')
lines(fitted(fitA80), col = 'blue')


fitA90 = arima(train90_hsi, order = c(1,1,0)) #AIC = -11989.5   BIC = -11972.76     11990/11973 = 1
tsdisplay(residuals(fitA90), lag.max = 40, main = 'Residuals (1,1,1)')
plot(hsi90_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(1,1,1)')
lines(fitted(fitA90), col = 'blue')


fitB70 = arima(train70_hsi, order = c(2,1,11)) #AIC = -11989.67  BIC = -11911.51    11990/11911 = 
tsdisplay(residuals(fitB70), lag.max = 40, main = 'Residuals (2,1,11)')
plot(hsi70_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(2,1,11)')
lines(fitted(fitB70), col = 'green')

fitB80 = arima(train80_hsi, order = c(2,1,11)) #AIC = -11989.67  BIC = -11911.51    11990/11911 = 
tsdisplay(residuals(fitB80), lag.max = 40, main = 'Residuals (2,1,11)')
plot(hsi80_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(2,1,11)')
lines(fitted(fitB80), col = 'green')

fitB90 = arima(train90_hsi, order = c(2,1,11)) #AIC = -11989.67  BIC = -11911.51    11990/11911 = 
tsdisplay(residuals(fitB90), lag.max = 40, main = 'Residuals (2,1,11)')
plot(hsi90_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(2,1,11)')
lines(fitted(fitB90), col = 'green')

#----------------------------------Runing Data using Auto ARIMA----------------------------------#
par(mfrow = c(2,2))
term = 738 #3 yrs
#term = 492 #2 yrs
#term = 246 #1 yr
fcast170 = forecast(fittrain70_hsi, h = term)
plot(fcast170, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast170_ext = as.numeric(fcast170$mean)
final_facast170 = exp(fcast170_ext)

fcast270 = forecast(fitA70, h = term)
plot(fcast270, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast270_ext = as.numeric(fcast270$mean)
final_facast270 = exp(fcast270_ext)

fcast370 = forecast(fitB70, h = term)
plot(fcast370, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast370_ext = as.numeric(fcast370$mean)
final_facast370 = exp(fcast370_ext)

par(mfrow = c(2,2))
term = 492 #2 yrs
#term = 246 #1 yr
fcast180 = forecast(fittrain80_hsi, h = term)
plot(fcast180, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast180_ext = as.numeric(fcast180$mean)
final_facast180 = exp(fcast180_ext)

fcast280 = forecast(fitA80, h = term)
plot(fcast280, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast280_ext = as.numeric(fcast280$mean)
final_facast280 = exp(fcast280_ext)

fcast380 = forecast(fitB80, h = term)
plot(fcast380, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast380_ext = as.numeric(fcast380$mean)
final_facast380 = exp(fcast380_ext)

par(mfrow = c(2,2))
term = 246 #1 yr
fcast190 = forecast(fittrain90_hsi, h = term)
plot(fcast190, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast190_ext = as.numeric(fcast190$mean)
final_facast190 = exp(fcast190_ext)

fcast290 = forecast(fitA90, h = term)
plot(fcast290, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast290_ext = as.numeric(fcast290$mean)
final_facast290 = exp(fcast290_ext)

fcast390 = forecast(fitB90, h = term)
plot(fcast390, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast390_ext = as.numeric(fcast390$mean)
final_facast390 = exp(fcast390_ext)


#------------------------------ Crate dataframe for actual and predicted prices -------------------------------#
df1 = data.frame(hsi[,2][1966:2457], final_facast1, hsi[,3][1966:2457])
#df1 = data.frame(hsi[,2][1966:2457], fcast1)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(df1) = col.headings


df2 = data.frame(hsi[,2][1966:2457], final_facast2, hsi[,3][1966:2457])
#df2 = data.frame(hsi[,2][1966:2457], fcast2)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(df2) = col.headings


df3 = data.frame(hsi[,2][1966:2457], final_facast380, hsi[,3][1966:2457])
#df3 = data.frame(hsi[,2][1966:2457], fcast3)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(df3) = col.headings

#------------------------------ Finding Mean Percentage Error(MPE) -------------------------------#
#由于公式中使用的是预测误差的实际值而不是绝对值，因此正预测误差和负预测误差可能会相互抵消。结果，该公式可以用作预测偏差的度量。
MPE1 = mean((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`)) #-6.52% deviation on average
MPE2 = mean((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`)) #-6.52% deviation on average
MPE3 = mean((df3$`Actual Price`-df3$`Forecasted Price`)/(df3$`Actual Price`)) #-6.39% deviation on average

#------------------------------ Finding Mean Absolute Percentage Error(MAPE) -------------------------------#
MAPE1 = mean(abs((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`))) #0.0807 -- 8.07%

MAPE2 = mean(abs((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`))) #0.0807 -- 8.07%

MAPE3 = mean(abs((df3$`Actual Price`-df3$`Forecasted Price`)/(df3$`Actual Price`))) #0.08 -- 8.00%

accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)

#------------------------------ Finding RMSE -------------------------------#
RMSE(log(df1$`Actual Price`), log(df1$`Forecasted Price`)) #0.0903
RMSE(log(df2$`Actual Price`), log(df2$`Forecasted Price`)) #0.0904
RMSE(log(df3$`Actual Price`), log(df3$`Forecasted Price`)) #0.0895

#------------------------------ Finding MASE -------------------------------#



#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  geom_line(aes(x = hsi$t, y = hsi$adjclose), colour = 'black') +
  geom_line(aes(x = df1$Days, y = df1$`Forecasted Price`), colour = 'blue') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')
  
ggplot() + 
  geom_line(aes(x = hsi$t[1:1965], y = hsi$adjclose[1:1965]), colour = 'red') +
  geom_line(aes(x = hsi$t[1966:2457], y = hsi$adjclose[1966:2457]), colour = 'orange') +
  geom_line(aes(x = df3$Days, y = df3$`Forecasted Price`), colour = 'purple') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')

#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
#Test whether there is sufficient structure in a time series to make it worth modelling
#Applied to residuals after fit to test whether there is remaining structure in the residuals
#Small P-values indicate significant autocorrelation
Box.test(fittrain_hsi$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_hsi$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fittrain_hsi$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 0.87, 0.1 and 0.09 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

Box.test(fitA$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 0.97, 0.11 and 0.99 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

Box.test(fitB$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitB$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fitB$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 1, 1 and 0.99 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

#-----------------------------------------Questions-------------------------------------------------#
#Why MPE calculated by self differs from the MPE in accuracy function???
#Any other methods to evaluate model???
