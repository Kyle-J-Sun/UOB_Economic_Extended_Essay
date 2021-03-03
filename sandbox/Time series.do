*-----------------------------------import data----------------------------------*
cd "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
import delimited using "HSI Daily.csv", clear varname(1)
destring open-volume, replace force
drop if close ==.

*----------------------------------date format-----------------------------------*
g daten = date(date, "YMD")
format daten %td
tsset daten, daily //We can't have gaps with time seires feature.
tsfill

*--------------------------Line graphs and tin()--------------------------*
use http://www.stata-press.com/data/r12/wpi1.dta, clear
tsset t, quarterly
twoway tsline wpi
twoway tsline wpi if tin(1970q1, 1979q4)

*--------------------------Time series operator--------------------------*
use http://www.stata-press.com/data/r12/wpi1.dta, clear
list t wpi in 1/12, sep(4) // List the observed data
list t wpi F1.wpi in 1/12, sep(4) //List Lead1 variable

gen wpi_F1 = F1.wpi //Lead1
gen wpi_F2 = F2.wpi //Lead2

gen wpi_L1 = L1.wpi //Lag1
gen wpi_L2 = L2.wpi //Lag2

gen wpi_D1 = D1.wpi //Difference1
gen wpi_D2 = D2.wpi //Difference2

gen wpi_D4 = D4.wpi //Seasonal Lag
gen wpi_S4 = S4.wpi //Seasonal Difference


*--------------------------Correlograms and Partial correlograms---------------*
use http://www.stata-press.com/data/r12/wpi1.dta, clear
tsset t, quarterly
twoway tsline wpi //Non-stationary
twoway tsline ln_wpi
twoway tsline D.ln_wpi
ac D.ln_wpi //shows the autocorrelation of D.ln_wpi
pac D.ln_wpi //partial autocorrelation with 95% CI


*-------------------------------------ARIMA model-------------------------------*
use http://www.stata-press.com/data/r12/wpi1.dta, clear
tsset t, quarterly
twoway tsline wpi //non-stationary not independent of time
twoway tsline ln_wpi
twoway tsline D.ln_wpi
ac D.ln_wpi
pac D.ln_wpi
arima wpi, arima(1,1,4) //p = 1, d = 1, q = 4

*-------------------------------------MA smoothers-------------------------------*
use http://www.stata-press.com/data/r12/sales1.dta, clear
tsset t
twoway tsline sales
tssmooth ma sales_ma212 = sales, window(2 1 2)
twoway tsline sales sales_ma*, legend(size(small))
tssmooth ma sales_wma121 = sales, weights(1 <2> 1) //W for lagged: 1  W for current obs: 2  W for lead: 1
twoway tsline sales sales_wma*, legend(size(small))
tssmooth ma sales_wma12221 = sales, weights(1,2 <2> 2,1) //W for lagged: 1,2  W for current obs: 2  W for lead: 2,1
twoway tsline sales sales_wma*, legend(size(small))
