library("class")


pca_simplification <- function(data, breakpoint=1, noPC=0) {
	if(breakpoint > 1){
		e <- simpleError(paste(c("Bad input. Too high breakpoint: ",breakpoint, " > 1."),collapse=""))
		stop(e)
	}
	
	data.pca = prcomp(na.pass(data$trainSet), center=TRUE, scale=FALSE)
	
	sdev_sum_sum = cumsum(data.pca$sdev^2 / sum(data.pca$sdev^2))
	NPC = noPC;
	if (noPC < 2 ) {
		NPC = length(sdev_sum_sum);
		for(i in 1:length(sdev_sum_sum) ){
			if ( sdev_sum_sum[i] >= breakpoint ) {
				NPC <- i
				break
			}
		}
		if(NPC == 1){ #this is weird... I will fix later
			NPC = 2
		}
	}
	train_data = data.pca$x[,1:NPC]
	test_data = ((data$testSet - data.pca$center) %*% data.pca$rotation)[,1:NPC]
	return(list(trainSet=train_data, testSet=test_data,trainVali=data$trainVali,testVali=data$testVali,variance=data.pca$sdev^2))
}

run_knn <- function(data,K) {
	res = knn(data$trainSet, data$testSet, data$trainVali, k = K)

	confus = array(0,c(10,10))
	
	per <- 0
	for(i in 1:length(data$testVali)){
		confus[[res[i],data$testVali[i]]] = confus[res[i],data$testVali[i]] + 1;
		if(res[i] == (data$testVali)[i]){
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

# # example run
# source("load_people_data.R")
# source("normalize.R")
# 
# data = prepareAllMixed(360,40)
# data = pca_simplification(data,breakpoint=.8)
# data = normalizeData(data, "z-score")
# # data = normalizeData(data, "min-max")
# print(run_knn(data,10))

