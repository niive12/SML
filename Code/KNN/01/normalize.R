# normalize the data...
library("gdata")


normalize <- function(x){
	return((x-min(x))/(max(x)-min(x)))
}

normalizeData <- function(data, normMethod = "min-max"){
	
	if(normMethod == "min-max"){
		trainS = lapply(data$trainSet,normalize)
		testS = lapply(data$testSet,normalize)
	}
	else if(normMethod == "z-score"){
		trainS = lapply(data$trainSet,scale)
		testS = lapply(data$testSet,scale)
	}
	
	return(list(trainSet = trainS, testSet = testS,trainVali=data$trainVali,testVali=data$testVali))
}

