library("C50")

random_forrest_predict <- function(model, data){
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