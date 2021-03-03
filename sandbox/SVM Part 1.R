#------------------------- Support Vector Machine (Classification) ---------------------------#
library(e1071)
plot(iris) #Iris dataset
iris

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)
plot(iris$Petal.Length, iris$Sepal.Width, col = iris$Species)

#------------------------- For linearly Separate ---------------------------#
s = sample(150,100)
col = c('Petal.Length', 'Petal.Width', 'Species')
iris_train = iris[s,col]
iris_test = iris[-s,col]

svmfit = svm(Species~., data = iris_train, kernel = 'linear', cost = .1, scale = F)
print(svmfit)
plot(svmfit, iris_train[,col])
?col
#Do cross-validation
tuned = tune(svm, Species~., data = iris_train, kernel = 'linear', ranges = list(cost = c(0.0001,0.01,0.1,1,10,100)))
summary(tuned)
#for summary, it shows that the best parameters should be 0.1

p = predict(svmfit, iris_test[,col], type = 'class')
plot(p)
table(p, iris_test[,3])
mean(p == iris_test[,3])
