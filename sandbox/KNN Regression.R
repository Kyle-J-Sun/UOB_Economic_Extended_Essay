fileName = "SP.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
SPinx = read.csv(fileLocation, header = T)
dim(SPinx)
View(SPinx)

n = nrow(SPinx)
#Set index as date
SPinx.ts = ts(data = SPinx[,6], deltat = 1/232, start = c(2018.3))
SPinx.ts
y = SPinx$Adj.Close
x = scale(SPinx[,6])
plot.ts(x, xlab = 'date', ylab = 'price')

install.packages('FNN')
library(FNN)

set.seed(1234)
test = sample(1:n, size = , replace = F)
cvErr = rep(x = 0, times = 12)
for (i in 1:12) {
  knnReg = knn.reg(train = x[-test,], y = y[-test], k = i)
  cvErr[i] = knnReg$PRESS / length(test)
}
plot(1:12, cvErr, type = 'b', xlab = 'K')

knnReg = knn.reg(train = x[-test,], test = x[test,], y = y[-test], k = 1)

dim(x[-test,])
