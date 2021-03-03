cd "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/" // cd data folder
import delimited using "df3.csv", clear varname(1) // importing data

// setting time series
tsset v1
g ar = d.actualprice/l.actualprice
g pr = d.forecastedprice/l.forecastedprice
g cp = (ar>0 & pr>0)
g cn = (ar<0 & pr<0)
g correct = cp + cn
sum correct

cd "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
import delimited using "table1.csv", clear varname(1)
tsset v1
g ar = d.actualprice/l.actualprice
g pr = d.forecastedprice/l.forecastedprice
g cp = (ar>0 & pr>0)
g cn = (ar<0 & pr<0)
g correct = cp + cn
sum correct
