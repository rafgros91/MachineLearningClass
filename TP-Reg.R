# TP-reg.R

	data(airquality)

# 
	summary(airquality)
# 
	pairs(airquality)
# 
	dim(airquality)
#
	names(airquality)
#
# Remove missing data ang get sample size and dimension
	DS = na.omit(airquality)
	dim(DS)
	n = nrow(DS)
	p = ncol(DS)

# First a Linear model
	mod1 = lm(Ozone ~ ., data=DS)
# plot(mod1) shows different plots 
	prev  = predict(mod1)

# Split the data in two subsamples
	Ind.test = sample(n,n/3)
	Learn = DS[-Ind.test,]
	Test = DS[Ind.test,]
# Check dimensions
	print(dim(Learn))
	print(dim(Test))

# Learn the model using the learning sample & predict over the test sample

	mod1 = lm(Ozone ~ ., data=Learn)
	prev = predict(mod1,newdata=Test)
# Analyze residuals
	summary(prev-Test$Ozone)
	plot(prev-Test$Ozone)

# compute Mean Squared error of prediction
	mse = mean((prev-Test$Ozone)^2)
# take its square root
	print(sqrt(mse))	

