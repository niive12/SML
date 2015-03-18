# normalize the data...


normalize <- function(x){
	return((x-min(x))/(max(x)-min(x)))
}

bin <- function(x, bins){
	min = min(x)
	max = max(x)
	binSize = (max-min)/bins
	
	for(feature in 1:length(x)){
		bin = 0
		while(x[feature] > (min + bin*binSize)){
			bin = bin +1
		}
		x[feature] = bin
	}
	
	return(x)
}


normalizeData <- function(data, normMethod = "min-max", bins = 2){ # data must be a list with a trainSet adn testSet entry
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
	}else if(normMethod == "bin"){
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] = bin(data$trainSet[i,], bins)
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] = bin(data$testSet[i,], bins)
		}
	}
	
	return(list(trainSet = trainS, testSet = testS,trainVali=data$trainVali,testVali=data$testVali))
}


# #  test of bin
# testarray = c(0.1,0.8,0.1,1,0,0.9,0.1,0.6,0.4,0.55)
# print(bin(testarray,3))