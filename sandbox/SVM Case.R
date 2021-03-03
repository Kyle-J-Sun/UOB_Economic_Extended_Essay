#We begin by generating the observations, 
#which belong to two classes, 
#and checking whether the classes are linearly separable.
set.seed(1)
x = matrix (rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1, ]= x[y==1, ] + 1
plot(x, col = (3-y))

#we must encode the response as a factor variable. 
#We now create a data frame with the response coded as a factor.
dat=data.frame(x=x, y=as.factor (y))
install.packages("e1071")
library (e1071)

#The argument scale=FALSE tells 
#the svm() function not to scale each feature to have mean zero or standard deviation one; 
#depending on the application, one might prefer to use scale=TRUE.
svmfit =svm(y~., data = dat, kernel = "linear", cost = 10, sacle = F)
plot(svmfit, dat)
svmfit$index #determine the suppot vectors
summary(svmfit) #there were seven support vectors, four in one class and three in the other.

#The e1071 library includes a built-in function, tune(), to perform crossvalidation. By default, 
#tune() performs ten-fold cross-validation on a set of models of interest.
set.seed (1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear" ,ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)#access the cross-validation errors
bestmod = tune.out$best.model
summary(bestmod)

#generating a test data set.
xtest=matrix (rnorm (20*2) , ncol =2)
ytest=sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame (x=xtest , y=as.factor (ytest))

#predict the class labels of these test observations.
ypred=predict (bestmod ,testdat)
table(predict = ypred , truth= testdat$y )

#We ﬁt the support vector classiﬁer and plot the resulting hyperplane, 
#using a very large value of cost so that no observations are misclassiﬁed.
dat=data.frame(x=x,y=as.factor (y))
svmfit = svm(y~., data = dat, kernel = 'linear', cost = 100000)
summary(svmfit)
plot(svmfit , dat)

svmfit = svm(y~., data = dat, kernel = 'linear', cost = 1)
summary(svmfit)
plot(svmfit, dat)
