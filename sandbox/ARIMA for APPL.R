#--------------------------------Loading Data----------------------------------------#
fileName = "AAPL (MONTH).csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
apple = read.csv(fileLocation, header = T)
dim(apple)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)

#-----------------------------Plot and convert to ln format-------------------------------------#
train_apple = log(apple[,6][1:144]) #build training data
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_apple,lag.max = 20)
# We have descent in our lags
pacf(train_apple, lag.max = 20) # Partial Autocorrelation Function
#First lag: Big Drop

#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_apple = diff(train_apple, 1)
adf.test(train_apple) #p = 0.1504: stationary at original time series
adf.test(difftrain_apple) # p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
apple_arima = ts(train_apple, start = c(2005,01), frequency = 12)
fittrain_apple = auto.arima(apple_arima)
par(mfrow = c(1,1))
plot(apple_arima, type = 'l')
title('Apple Price')
exp(train_apple)

#------------------------------Forecasting Data using Auto ARIMA-------------------------------#
Forecasted_Values_ln = forecast(fittrain_apple, h = 36)
plot(Forecasted_Values_ln)

Forecasted_Values_extracted_apple = as.numeric(Forecasted_Values_ln$mean)
Final_Forecated_Values_apple = exp(Forecasted_Values_extracted_apple) #converting back into normal format.
Final_Forecated_Values_apple

#------------------------------Finding Percentage Error-------------------------------#
df = data.frame(apple[,6][145:180], Final_Forecated_Values_apple)
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
Box.test(fittrain_apple$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_apple$residuals, lag = 10, type = 'Ljung-Box')
Box.test(fittrain_apple$residuals, lag = 15, type = 'Ljung-Box')
#p-value = 0.5, 0.7 and 0,6 which are all above 0.05 --- Fail to reject the null hypothesis that the residuals are random
