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
	data_pc = c(seq(2,80,2))
	pc_knn_success = matrix(0,2,length(data_pc))
	
	data_s = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, filter = "gaussian", sigma = s_sigma, size = s_size, make_new = 1)
	data_n = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, make_new = 1)
	
	for(f_pc in 1:length(data_pc)){
		print(paste(c("Running PC contour: ", f_pc, " / ", length(data_pc)), collapse = ""))
		data_sim_pc = pca_simplification(data_s, noPC=data_pc[f_pc]);
		
		knn_res = run_knn(data_sim_pc, s_k)
		
		pc_knn_success[1,f_pc] = knn_res$success
		
		data_sim_pc = pca_simplification(data_n, noPC=data_pc[f_pc]);
		
		knn_res = run_knn(data_sim_pc, s_k)
		
		pc_knn_success[2,f_pc] = knn_res$success
		
		save(data_pc, pc_knn_success, file = "KNN_final_pca.RData")
	}
	
}else{
	load("KNN_final_pca.RData")
}

col = rainbow(2)
setEPS()
postscript("../../../Report/graphics/knn_pc.eps",height = 4, width = 8)
plot(data_pc,pc_knn_success[1,],type="b",xlab="PC",ylab="Success",ylim = c(min(pc_knn_success), max(pc_knn_success)), col = col[1])
lines(data_pc,pc_knn_success[2,], type = "b", col = col[2])
legend("bottomright",c("Smoothed","Not Smoothed"),cex=0.8,lty=1:2, col = col)
quiet = dev.off()
