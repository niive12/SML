# test for the final report
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")


testSetSize = 40
trainSetSize = 360


# pca test,, comp sigma vs PC
if(T){
	s_k = 1
	s_size = 5
	s_sigma = 0.9
	data_pc = c(seq(2,50,2))
	pc_knn_success = c(1:length(data_pc))
	
	data = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, filter = "gaussian", sigma = s_sigma, size = s_size, make_new = 1)
	
	for(f_pc in 1:length(data_pc)){
		print(paste(c("Running PC contour: ", f_pc, " / ", length(data_pc)), collapse = ""))
		data_sim_pc = pca_simplification(data, noPC=data_pc[f_pc]);
		
		knn_res = run_knn(data_sim_pc, s_k)
		
		pc_knn_success[f_pc] = knn_res$success
		
		setEPS()
		postscript("../../../Report/graphics/knn_pc.eps",height = 4, width = 8)
		plot(data_pc[1:f_pc],pc_knn_success[1:f_pc],type="b",xlab="PC",ylab="Success")
		quiet = dev.off()
		save(data_pc, pc_knn_success, file = "KNN_final_pca.RData")
	}
	
}else{
	load("KNN_final_pca.RData")
	setEPS()
	postscript("../../../Report/graphics/knn_pc.eps",height = 4, width = 8)
	plot(data_pc,pc_knn_success,type="b",xlab="PC",ylab="Success")
	quiet = dev.off()
}

