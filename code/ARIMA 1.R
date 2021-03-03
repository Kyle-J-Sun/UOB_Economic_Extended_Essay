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
train_hsi = log(hsi[,2][1:1965])

#-----------------------------ACF, PACF and Dickey-Fuller Test for 3 yrs-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(train_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train_hsi)) 

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain_hsi = diff(train_hsi, 1)
adf.test(difftrain_hsi)
par(mfrow = c(1,2))

acf(difftrain_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(difftrain_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(difftrain_hsi)) 
#------------------------------Runing Data using Auto ARIMA-------------------------------#
hsi_arima = ts(exp(train_hsi), start = c(2010,1), frequency = 1)

#components.ts =decompose(hsi_arima)
#plot(components.ts)
#timeseriesseasonallyadjusted <- hsi_arima- components.ts$seasonal
#plot(timeseriesseasonallyadjusted)
#tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

fittrain_hsi = auto.arima(hsi_arima)
plot(hsi_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of auto ARIMA(0,1,0)')
lines(fitted(fittrain_hsi), col = 'red')

fitA = arima(train_hsi, order = c(1,1,0)) #AIC = -11991.5 -119.33 = 
BIC(fitA)
tsdisplay(residuals(fitA), lag.max = 40, main = 'Residuals (1,1,1)')
plot(hsi_arima, type = 'l', xlab = 'Days', ylab = "HSI Index Price (Log)", main = 'The fit of custom ARIMA(1,1,1)')
lines(fitted(fitA), col = 'blue')

accuracy(fittrain_hsi)
accuracy(fitA)




par(mfrow = c(2,2))
term = 492 #2 yrs
#term = 246 #1 yr
#term = 101
fcast1 = forecast(fittrain_hsi, h = term)
plot(fcast1, xlab = 'Days', ylab = 'HSI close price', main = 'ARIMA(0,1,0) Forecast for 2 years')
fcast1_ext = as.numeric(fcast1$mean)
final_facast1 = cast1_ext

fcast2 = forecast(fitA, h = term)
plot(fcast2, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast2_ext = as.numeric(fcast2$mean)
final_facast2 = exp(fcast2_ext)

#------------------------------ Crate dataframe for actual and predicted prices -------------------------------#
df1 = data.frame(hsi[,2][1966:2211], final_facast1, hsi[,3][1966:2211])
#df1 = data.frame(hsi[,2][1966:2457], fcast1)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(df1) = col.headings


df2 = data.frame(hsi[,2][1966:2457], final_facast2, hsi[,3][1966:2457])
#df2 = data.frame(hsi[,2][1966:2457], fcast2)
col.headings = c('Actual Price', 'Forecasted Price', 'Days')
names(df2) = col.headings

#------------------------------ Finding Mean Percentage Error(MPE) -------------------------------#
#由于公式中使用的是预测误差的实际值而不是绝对值，因此正预测误差和负预测误差可能会相互抵消。结果，该公式可以用作预测偏差的度量。
MPE1 = mean((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`)) #-6.52% deviation on average
MPE2 = mean((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`)) #-6.52% deviation on average
MPE1
#------------------------------ Finding Mean Absolute Percentage Error(MAPE) -------------------------------#
MAPE1 = mean(abs((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`))) #0.07 -- 8.07%

MAPE2 = mean(abs((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`))) #0.07 -- 8.07%

accuracy(fcast1)
accuracy(fcast2)

#------------------------------ Finding RMSE -------------------------------#
RMSE(df1$`Actual Price`, df1$`Forecasted Price`) #0.0903
RMSE(df2$`Actual Price`, df2$`Forecasted Price`) #0.0904

MAE(df1$`Actual Price`, df1$`Forecasted Price`)

#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  geom_line(aes(x = hsi$t, y = hsi$adjclose), colour = 'black') +
  geom_line(aes(x = df1$Days, y = df1$`Forecasted Price`), colour = 'blue') +
  geom_line(aes(x = df2$Days, y = df2$`Forecasted Price`), colour = 'yellow') +
  geom_line(aes(x = df3$Days, y = df3$`Forecasted Price`), colour = 'green') +
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

par(mfrow=c(1,1))
acf(fittrain_hsi$residuals)
#residual diagnostics
boxresult<- LjungBoxTest(fittrain_hsi$residuals,k=2,StartLag=1) # residual?? or the original series?
par(mfrow=c(2,1))
plot(boxresult[,3],main="Ljung-Box Q Test", ylab="P-values", xlab="Lag")
qqnorm(fittrain_hsi$residuals)
qqline(fittrain_hsi$residuals)

Box.test(fitA$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 0.97, 0.11 and 0.99 which are all above 0.05 --- 