install.packages('ISLR')
library(ISLR)
names(Smarket)
dim(Smarket)
View(Smarket)
summary(Smarket)

pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9]) #Showing the correlation between varaibles without direction
attach(Smarket)
plot(Volume) #little correlation between today's return and previous days' return

#Set training data and testing data:
train = (Year<2005) #create a vector corresponding to the observations from 2001 through 2004
Smarket.2005 = Smarket[!train,] #use last vector to create a held out data set of observations from 2005.

#K-NN algorithm:
install.packages('class')
library(class)
#cbind allows us to bind the lag1 and lag2 together into two matrices
train.X = cbind(Lag1, Lag2) [train,]  #Show previous 4 and 5 days' percentage return.
View(train.X)
test.X = cbind(Lag1, Lag2) [!train,]
train.Direction = Direction [train] #put train direction of train data into train.Direction
View(train.Direction)
#Predict
#Set random seed before as because if several observations are tied as nearest neihbours.
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
knn.Accuracy = (43+83)/252
knn.Accuracy
#Cross Validation
k_test = seq(3, 15, 2)
?seq
set.seed(1234)
err = rep(NA, 7)
for (i in 1:7){
  out = knn.cv(train.X, cl = train.Direction, k = k_test[i])
  err[i] = mean(out != !train.Direction)
}
plot(k_test, err, type = 'b', ylab = 'Error Rate', xlab = 'K')
knn.pred = knn(train.X, test.X, train.Direction, k = 11)
table(knn.pred, Direction.2005)
knn.Accuracy = (52+76)/(252)
knn.Accuracy

#Fit Logistic Regression 
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef[,4] #only show p-value of the summary
glm.probs = predict(glm.fits, type = 'response') #to predict the probability that the market will go up, given values of the predictors
glm.probs[1:10]
contrasts(Direction) #contrasts() function indicates that R has created a dummy variable with a 1 for Up
glm.pred = rep('Down', 1250) #creates a vector of 1,250 Down elements
glm.pred[glm.probs>.5] = 'Up'
table(glm.pred, Direction) #to produce a confusion matrix in order to determine how many observations were correctly or incorrectly classiﬁed.
(145+507)/1250
mean(glm.pred == Direction)
View(Smarket)
attach(Smarket)
train = (Year<2005) #create a vector corresponding to the observations from 2001 through 2004
Smarket.2005 = Smarket[!train,] #use last vector to create a held out data set of observations from 2005.
#Smarket[!train,] yields a submatrix of the stock market data containing only the observations for which train is FALSE, the observations with dates in 2005
dim(Smarket.2005)
Direction.2005 = Direction[!train] 

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = 'response')
glm.pred = rep('Down', 252)
glm.pred[glm.probs>.5] = 'Up'
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) #computes the test set error rate.

glm.fits = glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = 'response')
glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = 'Up'
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)
