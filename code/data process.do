cd "/Users/kyle/Documents/Economics Extended Essay/Data/HSI/"
import delimited using "HSIM.csv", clear varname(1)
g t = _n
tsset t
g return = d.close/l.close
