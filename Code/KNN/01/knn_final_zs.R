# test for the final report
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")



mode = 2 # 1 = one, 2 = all mixed, 3 = 1 v All
# 50 test 100 train, one alone
# 100 test 50 trin, all mixed

if(mode == 1){
	testSetSize = 40
	trainSetSize = 360
}else if (mode == 2){
	testSetSize = 100
	trainSetSize = 50
} else if(mode == 3){
	testSetSize = 50
	trainSetSize = 100
}

filename = paste(c("KNN_final_zscore_", mode, ".RData"), collapse = "")
graphname = paste(c("../../../Report/graphics/knn_zscore_", mode, ".eps"), collapse = "")

# pca test, comp sigma vs PC
if(F){
	s_k = 1
	s_size = 5
	s_sigma = 0.9
	s_pc = 40
	order = c("Raw", "S", "ZS", "PCA", "PCA + ZS", "ZS + PCA", "ZS + PCA + ZS", "S + ZS", "S + PCA", "S + PCA + ZS", "S + ZS + PCA", "S + ZS + PCA + ZS")
	
	# data = n
	if(mode == 1){
		data_n = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, make_new = 1)
	}else if (mode == 2){
		data_n = prepareAllMixed(trainPart = trainSetSize, testPart = testSetSize, make_new = 1)
	} else if(mode == 3){
		data_n = prepareOneAlone(3, 2, trainPartSize = trainSetSize, testSize = testSetSize, make_new = 1)
	}
	# - data = n + zscore
	data_n_zs = normalizeData(data_n, normMethod = "z-score")
	# - data = n + pc 
	data_n_pc = pca_simplification(data_n, noPC = s_pc)
	# - data = n + pc + zscore
	data_n_pc_zs = normalizeData(data_n_pc, normMethod = "z-score")
	# - data = n + zscore + pc
	data_n_zs_pc = pca_simplification(data_n_zs, noPC = s_pc)
	# - data = n + zscore + pc + zscore
	data_n_zs_pc_zs = normalizeData(data_n_zs_pc, normMethod = "z-score")
	
	
	# with smoothing
	if(mode == 1){
		data_s = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma)
	}else if (mode == 2){
		data_s = prepareAllMixed(trainPart = trainSetSize, testPart = testSetSize, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma)
	} else if(mode == 3){
		data_s = prepareOneAlone(3, 2, trainPartSize = trainSetSize, testSize = testSetSize, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma)
	}	
	# - data = s + zscore
	data_s_zs = normalizeData(data_s, normMethod = "z-score")
	# - data = s + pc 
	data_s_pc = pca_simplification(data_s, noPC = s_pc)
	# - data = s + pc + zscore
	data_s_pc_zs = normalizeData(data_s_pc, normMethod = "z-score")
	# - data = s + zscore + pc
	data_s_zs_pc = pca_simplification(data_s_zs, noPC = s_pc)
	# - data = s + zscore + pc + zscore
	data_s_zs_pc_zs = normalizeData(data_s_zs_pc, normMethod = "z-score")

	print("Datasets loaded.")
	
	knn_n 			= run_knn(data_n, s_k)$success
	knn_n_zs 		= run_knn(data_n_zs, s_k)$success
	knn_n_pc 		= run_knn(data_n_pc, s_k)$success
	print("Halfway through raw.")
	knn_n_pc_zs 	= run_knn(data_n_pc_zs, s_k)$success
	knn_n_zs_pc 	= run_knn(data_n_zs_pc, s_k)$success
	knn_n_zs_pc_zs  = run_knn(data_n_zs_pc_zs, s_k)$success
	print("Datasets without smoothing run.")
	knn_s 			= run_knn(data_s, s_k)$success
	knn_s_zs 		= run_knn(data_s_zs, s_k)$success
	knn_s_pc 		= run_knn(data_s_pc, s_k)$success
	print("Halfway through smooth.")
	knn_s_pc_zs 	= run_knn(data_s_pc_zs, s_k)$success
	knn_s_zs_pc 	= run_knn(data_s_zs_pc, s_k)$success
	knn_s_zs_pc_zs = run_knn(data_s_zs_pc_zs, s_k)$success
	
	order =  c("Raw", "ZS", "PCA", "PCA+ZS", "ZS+PCA", "ZS+PCA+ZS", "S", "S+ZS", "S+PCA", "S+PCA+ZS", "S+ZS+PCA", "S+ZS+PCA+ZS")
	success = c(knn_n, 
				knn_n_zs, knn_n_pc,
				knn_n_pc_zs, knn_n_zs_pc, knn_n_zs_pc_zs,
				knn_s,
				knn_s_zs, knn_s_pc,
				knn_s_pc_zs, knn_s_zs_pc, knn_s_zs_pc_zs)

	
	save(order, success, file = filename)
	
}else{
	load(filename)
	#	print(success)
}

	setEPS()
	postscript(graphname,height = 5, width = 8)
	par(mar=c(7,4,4,0))
	ylimits = c(round((2 * min(success) - max(success) - 0.05), digits = 1), round((max(success) + 0.05), digits = 1))
	if(ylimits[1] < 0){ylimits[1] = 0}
	barplot(rep(NA,length(success)),ylim=ylimits,axes=FALSE)
	barplot(success, ylab = "Success", names.arg=order, horiz=F,las=2, ylim=ylimits, add = T, xpd = F)
	abline(h=mean(success) ,ylim=ylimits)
	q = dev.off()


