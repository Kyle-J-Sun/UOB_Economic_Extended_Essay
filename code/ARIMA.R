#--------------------------------Loading Data----------------------------------------#
fileName = "HSIDD.csv"
defaultDataDir = "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/"
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
train70_hsi = log(hsi[,6][1:241])
#train_hsi = hsi[,2][1:1965]
#Stock prices are based on returns and returns are based on percentages
#So we convert data to log format to make sure that fash attribute


#-----------------------------ACF, PACF and Dickey-Fuller Test for 3 yrs-------------------------------------#
#To do this for the autocorrelation test
par(mfrow = c(1,2))
acf(train70_hsi, main = 'ACF test for training data') #we get q value from acf       
pacf(train70_hsi, main = 'PACF test for training data') #we get p value from pacf
print(adf.test(train70_hsi)) 

#------------------------Screening Stationarity looking at ACF PACF-------------------------------------#
difftrain70_hsi = diff(train70_hsi, 1) # d = 1
second_difftrain_hsi = diff(train70_hsi, 1, 2)
third_difftrain_hsi = diff(train70_hsi, 1, 3)
forth_difftrain_hsi = diff(train70_hsi, 1, 4)
adf.test(difftrain70_hsi)# p = 0.01: reject null hypothesis of non-stationarity at 5% level and conlude that our 
#Using DF test to screen stationarity across between original time series and differenced time series

#------------Get first order difference acf and pacf 
par(mfrow = c(1,2))
acf(difftrain70_hsi, main = 'ACF test for training data')      
pacf(difftrain70_hsi, main = 'PACF test for training data')

#------------Get second order difference acf and pacf 
adf.test(second_difftrain_hsi)
par(mfrow = c(1,2))
acf(second_difftrain_hsi, main = 'ACF test for training data')     
pacf(second_difftrain_hsi, main = 'PACF test for training data') 

#------------Get third order difference acf and pacf 
adf.test(third_difftrain_hsi)
par(mfrow = c(1,2))
acf(third_difftrain_hsi, main = 'ACF test for training data')     
pacf(third_difftrain_hsi, main = 'PACF test for training data') 

#------------Get forth order difference acf and pacf 
adf.test(forth_difftrain_hsi)
par(mfrow = c(1,2))
acf(forth_difftrain_hsi, main = 'ACF test for training data')     
pacf(forth_difftrain_hsi, main = 'PACF test for training data') 

#------------------------------Runing Data using Auto ARIMA-------------------------------#
hsi70_arima = ts(train70_hsi, start = c(1990,12), frequency = 12)
fittrain70_hsi = auto.arima(hsi70_arima) # AIC=-547.92 BIC=-537.48
tsdisplay(residuals(fittrain70_hsi), lag.max = 40, main = 'Auto')

fitA = arima(train70_hsi, order = c(11,4,3), optim.control = list(maxit = 1000))
tsdisplay(residuals(fitA), lag.max = 40, main = 'Residuals (14,1,11)')
#AIC = -534.26 BIC = -443.76
BIC(fitA)

#----------------------------------Runing Data using Auto ARIMA----------------------------------#
par(mfrow = c(1,2))
term = 3
fcast1 = forecast(fittrain70_hsi, h = term)
plot(fcast1, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast1_ext = as.numeric(fcast1$mean)
final_facast1 = exp(fcast1_ext)

fcast2 = forecast(fitA, h = term)
plot(fcast2, xlab = 'Days', ylab = 'Logarithm of HSI close price')
fcast2_ext = as.numeric(fcast2$mean)
final_facast2 = exp(fcast2_ext)

#------------------------------ Crate dataframe for actual and predicted prices -------------------------------#
df1 = data.frame(hsi[,6][242:244], final_facast1, hsi[,2][242:244])
#df1 = data.frame(hsi[,2][1966:2457], fcast1)
col.headings = c('Actual Price', 'Forecasted Price', 'Date')
names(df1) = col.headings


df2 = data.frame(hsi[,6][242:244], final_facast2, hsi[,2][242:244])
#df2 = data.frame(hsi[,2][1966:2457], fcast2)
col.headings = c('Actual Price', 'Forecasted Price', "Date")
names(df2) = col.headings

#------------------------------ Finding Mean Percentage Error(MPE) -------------------------------#
#由于公式中使用的是预测误差的实际值而不是绝对值，因此正预测误差和负预测误差可能会相互抵消。结果，该公式可以用作预测偏差的度量。
MPE1 = mean((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`)) #-6.52% deviation on average
MPE2 = mean((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`)) #-6.52% deviation on average

#------------------------------ Finding Mean Absolute Percentage Error(MAPE) -------------------------------#
mean(abs((df1$`Actual Price`-df1$`Forecasted Price`)/(df1$`Actual Price`))) #0.0807 -- 8.07%
mean(abs((df2$`Actual Price`-df2$`Forecasted Price`)/(df2$`Actual Price`))) #0.0807 -- 8.07%

#------------------------------ Finding RMSE -------------------------------#
RMSE(df1$`Actual Price`, df1$`Forecasted Price`) #0.0903
RMSE(df2$`Actual Price`, df2$`Forecasted Price`) #0.0904

#------------------------------ Finding MAE for 2yrs -------------------------------#
MAE(df1$`Actual Price`, df1$`Forecasted Price`)
MAE(df2$`Actual Price`, df2$`Forecasted Price`)

#---------------------------------Ljung-Box-----------------------------------#
#If there is correlation between our residuals, then it will cause the problem of the time series model
#the correlations between residuals could skew the accuracy of the estimates
#Our null hypothesis is our residuals are random
#Test whether there is sufficient structure in a time series to make it worth modelling
#Applied to residuals after fit to test whether there is remaining structure in the residuals
#Small P-values indicate significant autocorrelation
Box.test(fittrain70_hsi$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fittrain70_hsi$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fittrain70_hsi$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 0.90, 0.15 and 0.53 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

Box.test(fitA$residuals, lag = 5, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 15, type = 'Ljung-Box')
Box.test(fitA$residuals, lag = 30, type = 'Ljung-Box')
#p-value = 0.99, 1 and 1 which are all above 0.05 --- 
#Fail to reject the null hypothesis that the residuals are random at 5% significant level

#-----------------------------Visualiazation of prediction-------------------------------------#
ggplot() + 
  #geom_line(aes(x = hsi$Date2, y = hsi$Close), colour = 'black') +
  geom_line(aes(x = hsi$Date2[242:349], y = hsi$Close[242:349]), colour = 'orange') +
  geom_line(aes(x = df1$Date, y = df1$`Forecasted Price`), colour = 'blue') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')

ggplot() + 
  #geom_line(aes(x = hsi$Date2[1:241], y = hsi$Close[1:241]), colour = 'red') +
  geom_line(aes(x = hsi$Date2[242:349], y = hsi$Close[242:349]), colour = 'orange') +
  geom_line(aes(x = df2$Date, y = df2$`Forecasted Price`), colour = 'purple') +
  ggtitle('Stock prediction') +
  xlab('time') +
  ylab('Stock Price')

#------------------------Accuracy of Prediction------------------------#
Acc = data.frame(hsidd[,1][242:349], hsidd[,6][242:349], df1$`Forecasted Price`, df2$`Forecasted Price`, table_lin[,2], table_poly[,2], table_rad[,2])
col.headings = c('date', 'ap', 'fp_arima_010', 'fp_arima_14111','fp_lin','fp_poly', 'fp_rad')
names(Acc) = col.headings
write.csv(Acc, file = "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/Accuracy.csv")
