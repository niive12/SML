library("C50")

tree_predict <- function(model, data){
	# summary( model )

	predictions = predict(model, data$testSet)

	confus = array(0,c(10,10))
	per = 0
	for(i in 1:length(data$testVali)){
		confus[[predictions[i],data$testVali[i]]] = confus[predictions[i],data$testVali[i]] + 1;
		if(predictions[i] == (data$testVali)[i]){
			per = per + 1
		} 
	}
	per = per/length(data$testVali)

	trueDetections = array(0,10)
	noChars = 10
	for(i in 1:noChars){
		trueDetections[i] = (confus[i,i]/(length(data$testVali)/noChars))
	}
	variance   =  var(trueDetections)

	return(list(confusion_matrix = confus, success = per, var=variance))
}

tree_predict_train <- function(model, data){
	predictions = predict(model, data$trainSet)
	per = 0
	for(i in 1:length(data$trainVali)){
		if(predictions[i] == data$trainVali[i]){
			per = per + 1
		} 
	}
	per = per/length(data$trainVali)
	return(per)
}

prepare_data_for_tree <- function(data){
	len = dim(data$trainSet)[2]
	names = array("a",len)
	for ( i in 1:len) { names[i] = paste(c("PC",i),collapse="") }
	colnames(data$trainSet) = names
	colnames(data$testSet) = names
	return(data)
}