clear

capture cd "/Users/kyle/Documents/Economics Extended Essay/Data/"


***********The analysis for AAPL using Linear Regression****************

import delimited using AAPL.csv, clear

rename date date_stringg
rename adjclose APPL
*set index to time series
generate date = date(date_stringg, "YMD")
format date %td

sort date
drop date_stringg
g date_id = _n
keep APPL date_id date
drop if APPL ==.
tsset date

describe

list APPL L.APPL
gen lagAPPL = L.APPL

generate returnAPPL = log(APPL) - log(lagAPPL)
regress returnAPPL L.returnAPPL  //To do regression between today and yesterday.
scatter returnAPPL L.returnAPPL
sort date
save APPLreturn, replace

tsline returnAPPL

help tsline

****************************No Autocorrelation*********************************




