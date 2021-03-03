#--------------------------------Loading Data----------------------------------------#
fileName = "HSI Monthly.csv"
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

#-----------------------------Plot and convert to ln format-------------------------------------#
train_hsi = log(hsi[,6][1:84]) #build training data
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_hsi,lag.max = 20)
# We have descent in our lags
pacf(train_hsi, lag.max = 20) # Partial Autocorrelation Function
#First lag: Big Drop

#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_hsi = diff(train_hsi, 1)
adf.test(train_hsi) 
#p = 0.1504: stationary at original time series
adf.test(difftrain_hsi) 
# p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
hsi_arima = ts(train_hsi, start = c(2010,01), frequency = 12)
fittrain_hsi = auto.arima(hsi_arima)
par(mfrow = c(1,1))
plot(hsi_arima, type = 'l')
title('hsi Price')
exp(train_hsi)

#------------------------------Forecasting Data using Auto ARIMA-------------------------------#
Forecasted_Values_ln = forecast(fittrain_hsi, h = 36)
plot(Forecasted_Values_ln)

Forecasted_Values_extracted_hsi = as.numeric(Forecasted_Values_ln$mean)
Final_Forecated_Values_hsi = exp(Forecasted_Values_extracted_hsi) #converting back into normal format.
Final_Forecated_Values_hsi

#------------------------------Finding Percentage Error-------------------------------#
df = data.frame(hsi[,6][85:120], Final_Forecated_Values_hsi)
col.headings = c('Actual Price', 'Forecasted Price')
names(df) = col.headings
attach(df)
percentage.error = ((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
percentage.error
mean(percentage.error) #5% deviation on average.


#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
Box.test(fittrain_hsi$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_hsi$residuals, lag = 10, type = 'Ljung-Box')
Box.test(fittrain_hsi$residuals, lag = 15, type = 'Ljung-Box')
#p-value = 0.5, 0.7 and 0,6 which are all above 0.05 --- Fail to reject the null hypothesis that the residuals are random
