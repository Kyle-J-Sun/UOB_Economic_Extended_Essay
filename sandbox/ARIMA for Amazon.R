#--------------------------------Loading Data----------------------------------------#
fileName = "AMZN (month).csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
amazon = read.csv(fileLocation, header = T)
dim(amazon)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)

#-----------------------------Plot and convert to ln format-------------------------------------#
train_amazon = log(amazon[,6][1:144]) #build training data
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_amazon,lag.max = 20)
# We have descent in our lags
pacf(train_amazon, lag.max = 20) # Partial Autocorrelation Function
#First lag: Big Drop

#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_amazon = diff(train_amazon, 1)
adf.test(train_amazon) #p = 0.0383: stationary at original time series
adf.test(difftrain_amazon) # p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
amazon_arima = ts(train_amazon, start = c(2005,01), frequency = 12)
fittrain_amazon = auto.arima(amazon_arima)
par(mfrow = c(1,1))
plot(amazon_arima, type = 'l')
title('Amazon Price')
exp(train_amazon)

#------------------------------Forecasting Data using Auto ARIMA-------------------------------#
Forecasted_Values_amazon_ln = forecast(fittrain_amazon, h = 36)
Forecasted_Values_amazon_ln
plot(Forecasted_Values_amazon_ln)

Forecasted_Values_extracted_amazon = as.numeric(Forecasted_Values_amazon_ln$mean)
Final_Forecated_Values_amazon = exp(Forecasted_Values_extracted_amazon) #converting back into normal format.
Final_Forecated_Values_amazon

#------------------------------Finding Percentage Error-------------------------------#
df = data.frame(amazon[,6][145:180], Final_Forecated_Values_amazon)
col.headings = c('Actual Price', 'Forecasted Price')
names(df) = col.headings
attach(df)
percentage.error = ((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
percentage.error
mean(percentage.error) #18% deviation on average.

#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
Box.test(fittrain_amazon$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain_amazon$residuals, lag = 10, type = 'Ljung-Box')
Box.test(fittrain_amazon$residuals, lag = 15, type = 'Ljung-Box')
#p-value = 0.5, 0.7 and 0,6 which are all above 0.05 --- Fail to reject the null hypothesis that the residuals are random

