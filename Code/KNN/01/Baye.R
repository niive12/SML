library("e1071")
library("gmodels")


baye_predict <- function(data, laplace = 0){
	print("Naive Bayes")
	classifier <- naiveBayes(x = data$trainSet, y = as.factor(data$trainVali))
	
	print("Prediction")
	predictions <- predict(classifier, data$testSet, type = "class")
		
	print("Cross Table")
	CrossTable(predictions,as.factor(data$testVali), prop.t=FALSE, prop.chisq=FALSE, dnn =c("Predicted","Actual"))
	
# 	CrossTable(predictions, data$testVali)
}

#example run
source("load_people_data.R")
source("normalize.R")
source("pca_test.R")

data = prepareAllMixed(400,400,peopleToLoad = getPeople())
data = normalizeData(data, "z-score")
data = pca_simplification(data,breakpoint=0.8)
data = normalizeData(data,"bin", 2)

baye_predict(data)


# print(predictions)