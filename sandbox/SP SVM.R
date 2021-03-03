#--------------------------------Loading Data----------------------------------------#
library(e1071)
fileName = "S&P daily with IV.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/S&P 500/"
fileLocation = file.path(defaultDataDir, fileName)
linsp = read.csv(fileLocation, header = T)
dim(linsp)

train = head(linsp, 1662)
train.X = train[4:11]
holdout = linsp[!train,]
holdout.X = linsp[4:11][!train,]
