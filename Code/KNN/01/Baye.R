library("e1071")
# library("gmodels")


baye_predict <- function(data, laplace = 0){
	# print("Naive Bayes")
	classifier <- naiveBayes(x = data$trainSet, y = as.factor(data$trainVali))
	
	# print("Prediction")
	predictions <- predict(classifier, data$testSet, type = "class")
	
	confus = array(0,c(10,10))
	per <- 0
	for(i in 1:length(data$testVali)){
		confus[[predictions[i],data$testVali[i]]] = confus[predictions[i],data$testVali[i]] + 1;
		if(predictions[i] == (data$testVali)[i]){
			per <- per + 1
		} 
	}
	per = per/length(data$testVali)
	
	trueDetections = array(0,10)
	noChars = 10
	for(i in 1:noChars){
		trueDetections[i] <- (confus[i,i]/(length(data$testVali)/noChars))
	}
	variance   =  var(trueDetections)
	
	return(list(confusion_matrix = confus, success = per, var=variance))
}

#example run
# source("load_people_data.R")
# source("normalize.R")
# source("pca_test.R")
# 
# data = prepareAllMixed(400,400,peopleToLoad = getPeople())
# data = normalizeData(data, "z-score")
# data = pca_simplification(data,breakpoint=0.8)
# data = normalizeData(data,"bin", 2)
# 
# baye_predict(data)


# print(predictions)