cd "/Users/kyle/Documents/UOB/Economics Extended Essay/Data/NEW/"
import delimited using "Accuracy2.csv", clear varname(1)

tsset weeks


g cp1 = (ap_dir == "Up" & arima_dir == "Up")
g cn1 = (ap_dir  == "Down" & arima_dir == "Down")
g correct = cp1 + cn1
sum correct

g cp2 = (ap_dir == "Up" & lin_dir == "Up")
g cn2 = (ap_dir == "Down" & lin_dir == "Down")
g correct2 = cp2 + cn2
sum correct2

g cp3 = (ap_dir == "Up" & rad_dir == "Up")
g cn3 = (ap_dir == "Down" & rad_dir == "Down")
g correct3 = cp3 + cn3
sum correct3


drop cp1-correct

drop if weeks > 758
