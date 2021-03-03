#-----------------------------Loading Libraries-------------------------------------#
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

#--------------------------Loading data from Yahoo Finance------------------------------#
getSymbols('SPY', from = '2015-01-01', to = '2020-01-01')
class(SPY)
#xts: it has data column and then you have an index field which has your dates in it
SPY_Close_Prices =  SPY[,4]
plot(SPY_Close_Prices)
class(SPY_Close_Prices)

par(mfrow = c(1,2))
Acf(SPY_Close_Prices, main = 'ACF')
Pacf(SPY_Close_Prices, main = "PACF")

print(adf.test(SPY_Close_Prices)) #p = 0.2491
auto.arima(SPY_Close_Prices, seasonal = F)

#-------------------------ADF test for p-value------------------------------#

fitA = auto.arima(SPY_Close_Prices, seasonal = F)
tsdisplay(residuals(fitA), lag.max = 40, main = '(2,1,0) Model Residuals') #Take this into graphs
#It gives you model residuals, ACF and PACF graphs
#AIC/BIC = 5384/5405 = 0.996 (this to be as low as possible)
#For lag.max I can change it higher or lower if I wanted to
auto.arima(SPY_Close_Prices, seasonal = F)

fitB = arima(SPY_Close_Prices,order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max = 40, main = '(1,2,4) Model Residuals')

fitC = arima(SPY_Close_Prices, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max = 40, main = '(5,1,4) Model Residuals')

fitD = arima(SPY_Close_Prices, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max = 40, main = '(1,1,1) Model Residuals')

#-------------------------Plots of ARIMA models------------------------------#
par(mfrow = c(2,2))

#------------------------auto arima (2,1,0)------------------------------#
term = 100
forecastA = forecast(fitA, h = term)
plot(forecastA)

#------------------------custom arima (3,0,3)------------------------------#
forecastB = forecast(fitB, h = term)
plot(forecastB)
forecastC = forecast(fitC, h = term)
plot(forecastC)
forecastD = forecast(fitD, h = term)
plot(forecastD)

#------------------------MAPE accuracy subtract from 100------------------------------#
#MAPE: the mean of percentage error
accuracy(forecastA)
accuracy(forecastB)
accuracy(forecastC)
accuracy(forecastD)
