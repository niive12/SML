# test for the final report
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("confusion_matrix.R")


testSetSize = 400
trainSetSize = 400


# pca test,, comp sigma vs PC
if(T){
	s_k = 1
	s_size = 5
	s_sigma = 0.9
	s_pc = 40
	
	# data 
	data_easy = prepareAllMixed(trainPart = trainSetSize*0.9, testPart = testSetSize*0.1, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma)
	data_easy = normalizeData(data_easy, normMethod = "z-score")
	data_easy = pca_simplification(data_easy, noPC = s_pc)
	data_easy = normalizeData(data_easy, normMethod = "z-score")
	
	knn_easy = run_knn(data_easy, s_k)
	
	# data 
	data_hard = prepareOneAlone(3, 2,  trainPartSize = trainSetSize, testSize = testSetSize, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma)
	data_hard = normalizeData(data_hard, normMethod = "z-score")
	data_hard = pca_simplification(data_hard, noPC = s_pc)
	data_hard = normalizeData(data_hard, normMethod = "z-score")
	
	knn_hard = run_knn(data_hard, s_k)
	
	#save adata
	save(knn_easy, knn_hard, file = "KNN_final_confus.RData")
	
	
}else{
	load("KNN_final_confus.RData")
}

	# print
	confusion_matrix(knn_easy$confus, filename="../../../Report/graphics/knn_confusion_bestparam_easy.eps")
	confusion_matrix(knn_hard$confus, filename="../../../Report/graphics/knn_confusion_bestparam_hard.eps")

	
	
	