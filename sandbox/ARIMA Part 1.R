#-----------------------------Loading Libraries-------------------------------------#
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(rio)
library(readxl)



#--------------------convert the date  into  a  date class-------------------------------------#
fileName = "AAPL.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
AAPL = read.csv(fileLocation, header = T)

AAPL$Date = as.Date(AAPL$Date)
ggplot(AAPL, aes())
#-----------------------------Loading Libraries-------------------------------------#
#-----------------------------Loading Libraries-------------------------------------#
#-----------------------------Loading Libraries-------------------------------------#
#-----------------------------Loading Libraries-------------------------------------#
#-----------------------------Loading Libraries-------------------------------------#
#-----------------------------Loading Libraries-------------------------------------#
