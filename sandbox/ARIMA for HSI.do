*-----------------------------------import data----------------------------------*
cd "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/"
import delimited using "Closeweek.csv", clear varname(1)
drop if close ==.

*----------------------------------date format-----------------------------------*
g daten = date(date, "YMD")
format daten %td
tsset weeks    //We can't have gaps with time seires feature.
//tsfill

*--------------------------Line graphs and tin()--------------------------*
twoway tsline close
g train_data = log(close) if weeks < 707
g test_data = log(close) if weeks > 706

*--------------------------acf and pacf test--------------------------*
ac D2.train_data
pac D2.train_data

arima train_data, arima(5,2,1)


*--------------------------Forecasting--------------------------*
predict yhat, dynamic(1718)
g exyhat = exp(yhat)
g extdata = exp(test_data) if t > 1719
tsline extdata exyhat if t>1719, xline(1719)
gen fe = extdata - exyhat
gen fse = fe^2


