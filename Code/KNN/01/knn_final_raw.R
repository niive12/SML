# test for the final report

source("load_people_data.R")
source("pca_test.R")


testSetSize = 40
trainSetSize = 360


# raw test
if(T){
	data = prepareOne(3, 2, trainSetSize, testSetSize, make_new = 1)
	
	datapoints = c(1:15)#,seq(10,20,5))
	raw_knn_success = c(1:length(datapoints))
	
	for(k in 1:length(datapoints)){
		print(paste(c("Running k: ", k, " / ", length(datapoints)), collapse = ""))
		knn_res = run_knn(data, datapoints[k])
		
		raw_knn_success[k] = knn_res$success
	}
	
	save(datapoints, raw_knn_success, file = "KNN_final_raw.RData")
}else{
	load("KNN_final_raw.RData")
}

	setEPS()
	postscript("../../../Report/graphics/knn_raw_success.eps",height = 4, width = 8)
	plot(datapoints,raw_knn_success,type="b",xlab="K",ylab="Success")
	quiet = dev.off()