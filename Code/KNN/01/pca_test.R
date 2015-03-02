source("load_people_data.R")


pca_func_dims <- function(data, breakpoint, k) {
	data.pca = prcomp(data$trainSet, center=TRUE, scale=FALSE)

	sdev_sum_sum = cumsum(data.pca$sdev^2 / sum(data.pca$sdev^2))
	
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

	train_data = data.pca$x[,1:NPC]
	

	test_data = ((data$testSet - data.pca$center) %*% data.pca$rotation)[,1:NPC]

	res = knn(train_data, test_data, data$trainVali, k = k)

	# count right detections

	confus = array(0,c(10,10))

	per <- 0
	for(i in 1:length(data$testVali)){
		confus[[data$testVali[i],res[i]]] = confus[data$testVali[i],res[i]] + 1;
		if(res[i] == (data$testVali)[i]){
			per <- per + 1
		} 
	}
	per = per/length(data$testVali)

	return(list(confusion_matrix = confus, success = per))
}

data = prepareAllMixed(360,40)
print(pca_func_dims(data,0.8,10))