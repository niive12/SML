# normalize the data...


normalize <- function(x){
	return((x-min(x))/(max(x)-min(x)))
}

normalizeData <- function(data, normMethod = "min-max"){
	trainS = matrix(,dim(data$trainSet)[1],dim(data$trainSet)[2])
	testS = matrix(,dim(data$testSet)[1],dim(data$testSet)[2])
	
	if(normMethod == "min-max"){
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] = normalize(data$trainSet[i,])
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] = normalize(data$testSet[i,])
		}
	}
	else if(normMethod == "z-score"){
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] = scale(data$trainSet[i,], scale = TRUE)
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] = scale(data$testSet[i,], scale = TRUE)
		}
	}
	
	return(list(trainSet = trainS, testSet = testS,trainVali=data$trainVali,testVali=data$testVali))
}

