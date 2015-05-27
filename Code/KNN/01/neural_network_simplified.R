library("RSNNS")

neural_network_simplification <- function(data, size=400) {
	if( 0 ){
		e <- simpleError("Bad input. Friendship is not magic")
		stop(e)
	}
	new_train_Vali = matrix(1,length(data$trainVali),10)
	
	for( i in 1:length(data$trainVali) ){
		for( j in 1:10 ){
			if( data$trainVali[i] != j ) {
				new_train_Vali[i,j] = 0;
			}
		}
	}
	
	model = list()
	
	time_start = proc.time()
	for(i in 1:10){
		model[[i]] = mlp(data$trainSet, new_train_Vali[,i], size=size, learnFunc="Std_Backpropagation", learnFuncParams=c(0.02,0,0,0,0), maxit=40)
		time = (proc.time()-time_start)[["user.self"]]
		print(paste(c("model : ",i, " time spent so far: ",time, " seconds"),collapse=""))
	}
	return(model)
}

neural_network_predict <- function(model, data){
	table_name = "test_table.tex"
	noChars = 10
	value = array(0,noChars)
	confus = array(0,c(noChars,noChars))
	per = 0
	for( i in 1:length(data$testVali)){
		for( j in 1:noChars){
			value[j] = predict(model[[j]],data$testSet[i,])
		}
		result = which(max(value)==value)
# 		print(c(i,result, value))
		confus[[ result, data$testVali[i] ]] = confus[[ result, data$testVali[i] ]] + 1;
		if( result == data$testVali[i]){
			per = per + 1;
		}
		write.latex(confus, 0:9, 0:9, table_name);
	}
	
	per = per/length(data$testVali)
	
	trueDetections = array(0,noChars)
	for(i in 1:noChars){
		trueDetections[i] = (confus[i,i]/(length(data$testVali)/noChars))
	}
	variance =  var(trueDetections)

	return(list(confusion_matrix = confus, success = per, var=variance))
}
