# TP-Class.R
# Experiment 4 classification models over the iris data set
# Logistic, CART, random Forest and knn.
	data(iris)
# 
	summary(iris)
# 
	pairs(iris[,-5],col=iris[,5])
# 
	dim(iris)
#
	names(iris)
#
	n = nrow(iris)
	p = ncol(iris)
	
# Split the data in two subsamples, n/3 2n/3 approximately

	Ind.test = c(sample(1:50,n/9),sample(51:100,n/9),sample(101:150,n/9))
	Learn = iris[-Ind.test,]
	Test = iris[Ind.test,]

# Check dimensions
	print(dim(Learn))
	print(dim(Test))

###############################
# The logisitic model 

# Learn the model using the learning sample & predict over the test sample
	library(nnet)

	mod1 = multinom(Species ~ ., data=Learn)
	summary(mod1)	

	prev = predict(mod1,newdata=Test)
# Analyze predictions 
	table(prev,Test$Species)
# compute the missclassification error
	err.logistic = 100 * mean(prev != Test$Species)

###############################
# CART

	library(rpart)
	mod2 = rpart(Species ~.,data=iris)
# png("Iris.tree.png")
	plot(mod2,branch=.5)
	text(mod2,cex=0.8)
	title("Classification tree for Iris data set",cex.main=0.8)
# dev.off()

# Now learn the tree using the leraning sample and compute prediction for test sample
	mod2 = rpart(Species ~.,data=Learn)
	prev = predict(mod2,newdata=Test)
# See the content of prev... For each observation in the test
# it returns probabilities for each possible output label
	prev = predict(mod2,newdata=Test,type="class")
	table(prev,Test$Species)	
	err.cart = 100 * mean(prev != Test$Species)

###############################
# Lets Try random forests

	library(randomForest)
	mod3 = randomForest(Species ~.,data=Learn)
	prev = predict(mod3,newdata=Test)
# See the content of prev... For each observation in the test
# it returns probabilities for each possible output label
	prev = predict(mod3,newdata=Test,type="class")
	table(prev,Test$Species)	
	err.rf = 100 * mean(prev != Test$Species)

###############################
# knn

# knn is quite different..
	library(class)
	prev = knn(Learn[,-5],Test[,-5],Learn[,5],k=3)
	table(prev,Test$Species)	
	err.knn = 100 * mean(prev != Test$Species)

	
	erreurs = c(err.logistic,err.cart, err.rf,err.knn)
