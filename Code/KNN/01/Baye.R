library("e1071")
library("gmodels")


baye_predict <- function(data, laplace = 0){
	classifier = naiveBayes(data$trainSet, data$trainVali, laplace=laplace)
	
	predictions = predict(classifier, data$testSet, type="class")
	
# 	CrossTable(predictions, data$testVali)
}

#example run
source("load_people_data.R")
source("normalize.R")
source("pca_test.R")

data = prepareAllMixed(360,40)
data = normalizeData(data, "z-score")
data = pca_simplification(data,breakpoint=.8)
data$trainSet = bin(data$trainSet, 2)
baye_predict(data, 1)