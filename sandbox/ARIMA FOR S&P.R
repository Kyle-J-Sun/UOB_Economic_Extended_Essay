#--------------------------------Loading Data----------------------------------------#
fileName = "S&P Monthly.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/S&P 500/"
fileLocation = file.path(defaultDataDir, fileName)
SP = read.csv(fileLocation, header = T)
dim(SP)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)

#-----------------------------Plot and convert to ln format-------------------------------------#
train_sp = log(SP[,6][1:84]) #build training data
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_sp,lag.max = 20)
# We have descent in our lags
pacf(train_sp, lag.max = 20) # Partial Autocorrelation Function
#First lag: Big Drop

#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_sp = diff(train_sp, 1)
adf.test(train_sp) #p = 0.1504: stationary at original time series
adf.test(difftrain_sp) # p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
sp_arima = ts(train_sp, start = c(2010,01), frequency = 12)
fittrain_sp = auto.arima(sp_arima)
par(mfrow = c(1,1))
plot(sp_arima, type = 'l')
title('sp Price')
exp(train_sp)

#------------------------------Forecasting Data using Auto ARIMA-------------------------------#
Forecasted_Values_ln = forecast(fittrain_sp, h = 36)
plot(Forecasted_Values_ln)

Forecasted_Values_extracted_sp = as.numeric(Forecasted_Values_ln$mean)
Final_Forecated_Values_sp = exp(Forecasted_Values_extracted_sp) #converting back into normal format.
Final_Forecated_Values_sp

#------------------------------Finding Percentage Error-------------------------------#
df = data.frame(SP[,6][85:120], Final_Forecated_Values_sp)
col.headings = c('Actual Price', 'Forecasted Price')
attach(df)
names(df) = col.headings
percentage.error = ((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
percentage.error
mean(percentage.error) #5% deviation on average.


#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
Box.test(fittrain_sp$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_sp$residuals, lag = 10, type = 'Ljung-Box')
Box.test(fittrain_sp$residuals, lag = 15, type = 'Ljung-Box')
#p-value = 0.5, 0.7 and 0,6 which are all above 0.05 --- Fail to reject the null hypothesis that the residuals are random
