#--------------------------Loading Data--------------------------------#
library(ISLR)
fileName = "amazon.csv"
defaultDataDir = "/Users/kyle/Documents/Economics Extended Essay/Data/"
fileLocation = file.path(defaultDataDir, fileName)
AMZ = read.csv(fileLocation, header = T)
dim(AMZ)

#--------------------------Compare correlation--------------------------------#
pairs(AMZ)
cor(AMZ)
cor(AMZ[,-9])

#--------------------------Logistic Regression--------------------------------#
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = AMZ, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(AMZ$Direction)
glm.pred = rep('DOWN',1250)
glm.pred[glm.probs>.5] = "UP"
table(glm.pred, AMZ$Direction)
(23+675)/(23+675+13+540)
mean(glm.pred==AMZ$Direction)


#--------------------------Train and Test Data--------------------------------#
train = (AMZ$Year < 2019)
AMZ.2019 = AMZ[!train,]
dim(AMZ.2019)
Direction.2019 = AMZ$Direction[!train]

glm.fits = glm(Direction~Lag2+Lag3+Lag4, data = AMZ, family = binomial, subset = train)
glm.probs = predict(glm.fits, AMZ.2019, type = 'response')
glm.pred = rep("DOWN", 252)
glm.pred[glm.probs>.5] = "UP"
table(glm.pred, Direction.2019)
mean(glm.pred == Direction.2019)
135/252

predict(glm.fits, newdata = data.frame(Lag1 = c(0.01,0.03), Lag2 = c(0.5,-0.8), Lag3 = c(0.001,0.5), Lag4 = c(-0.4,0.002)), type = 'response')
