#--------------------------------Loading Data----------------------------------------#
fileName = "AAPL (MONTH).csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
APPL = read.csv(fileLocation, header = T)
dim(APPL)

#-----------------------------Loading Libraries-------------------------------------#
library(MASS)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)

#-----------------------------Plot and convert to ln format-------------------------------------#
train = log(APPL[,6][1:144]) #build training data
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute

#-----------------------------ACF, PACF and Dickey-Fuller Test-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train,lag.max = 20)
# We have descent in our lags
pacf(train, lag.max = 20) # Partial Autocorrelation Function
#First lag: Big Drop

#Those two above indicates that our data are stationary and usable for ARIMA
#If we don't have stationary model, no way to have an accurate prediction.

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain = diff(train, 1)
difftrain
adf.test(train) #p = 0.744: stationary at original time series
adf.test(difftrain) # p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------------------------Runing Data using Auto ARIMA-------------------------------#
APPLarima = ts(train, start = c(2005,01), frequency = 12)
fittrain = auto.arima(APPLarima)
par(mfrow = c(1,1))
plot(APPLarima, type = 'l')
title('APPL Price')
exp(train)

#------------------------------Forecasting Data using Auto ARIMA-------------------------------#
ForecastedValues.ln = forecast(fittrain, h = 36)
ForecastedValues.ln
plot(ForecastedValues.ln)

ForecastedValues.extracted = as.numeric(ForecastedValues.ln$mean)
FinalForecatedValues = exp(ForecastedValues.extracted) #converting back into normal format.
FinalForecatedValues

#------------------------------Finding Percentage Error-------------------------------#
df = data.frame(APPL[,6][145:180], FinalForecatedValues)
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
Box.test(fittrain$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain$residuals, lag = 10, type = 'Ljung-Box')
Box.test(fittrain$residuals, lag = 15, type = 'Ljung-Box')
#p-value = 0.5, 0.7 and 0,6 which are all above 0.05 --- Fail to reject the null hypothesis that the residuals are random
