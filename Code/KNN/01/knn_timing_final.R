#time knn pca vs not

source("load_people_data.R")
source("normalize.R")
source("pca_test.R")

fileName <- "final_knn_timing.RData"

if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	if(file.exists(fileName)){
		load(fileName)
	}
	data_knn_s = prepareOneAlone(3,2,trainPartSize=400, testSize=400, filter = "gaussian", size = 5, sigma = 0.9, make_new = 0)
	data_knn_c = data_knn_s
	
	# prepare...
	trainS = matrix(,dim(data_knn_c$trainSet)[1],dim(data_knn_c$trainSet)[2])
	#z-score
	for(i in 1:dim(data_knn_c$trainSet)[1]){
		trainS[i,] = scale(data_knn_c$trainSet[i,], scale = TRUE)
	}
	data_knn_c$trainSet = trainS
	#pca
	data.pca = prcomp(na.pass(data_knn_c$trainSet), center=TRUE, scale=FALSE)
	data_knn_c$trainSet = data.pca$x[,1:40]
	trainS = matrix(,dim(data_knn_c$trainSet)[1],dim(data_knn_c$trainSet)[2])
	#z-score
	for(i in 1:dim(data_knn_c$trainSet)[1]){
		trainS[i,] = scale(data_knn_c$trainSet[i,], scale = TRUE)
	}
	data_knn_c$trainSet = trainS
	
# 	trainS = matrix(,dim(data_knn_s$trainSet)[1],dim(data_knn_s$trainSet)[2])
# 	#z-score
# 	for(i in 1:dim(data_knn_s$trainSet)[1]){
# 		trainS[i,] = scale(data_knn_s$trainSet[i,], scale = TRUE)
# 	}
# 	data_knn_s$trainSet = trainS
	
	
	
	
	testS = matrix(,dim(data_knn_c$testSet)[1],dim(data_knn_c$testSet)[2])
	# c
	pre_process_time_start_knn = proc.time()
	#z-score
	for(i in 1:dim(data_knn_c$testSet)[1]){
		testS[i,] = scale(data_knn_c$testSet[i,], scale = TRUE)
	}
	data_knn_c$testSet = testS
	data_knn_c$testSet = ((data_knn_c$testSet - data.pca$center) %*% data.pca$rotation)[,1:40]
	#zscore
	testS = matrix(,dim(data_knn_c$testSet)[1],dim(data_knn_c$testSet)[2])
	for(i in 1:dim(data_knn_c$testSet)[1]){
		testS[i,] = scale(data_knn_c$testSet[i,], scale = TRUE)
	}
	data_knn_c$testSet = testS
	pre_process_time_knn_c = (proc.time() - pre_process_time_start_knn)[["user.self"]]
	
	testS = matrix(,dim(data_knn_s$testSet)[1],dim(data_knn_s$testSet)[2])
	
	# s
# 	pre_process_time_start_knn = proc.time()
# 	#z-score
# 	for(i in 1:dim(data_knn_s$testSet)[1]){
# 		testS[i,] = scale(data_knn_s$testSet[i,], scale = TRUE)
# 	}
# 	data_knn_s$testSet = testS
# 	pre_process_time_knn_s = (proc.time() - pre_process_time_start_knn)[["user.self"]]
	
	
	
# 	classification_time_start = proc.time()
# 	knn_success_s = run_knn(data_knn_s, 1)$success
# 	classification_time_knn_s = (proc.time() - classification_time_start)[["user.self"]]

	
	
	classification_time_start = proc.time()
	knn_success_c = run_knn(data_knn_c, 1)$success
	classification_time_knn_c = (proc.time() - classification_time_start)[["user.self"]]
	
	
	
	save(pre_process_time_knn_c, pre_process_time_knn_s, classification_time_knn_c, classification_time_knn_s, knn_success_c, knn_success_s,file=fileName)
}


colors = rainbow(3);

pre_process = c(pre_process_time_knn_s,pre_process_time_knn_c)
training = c(0,0)
classification = c(classification_time_knn_s,classification_time_knn_c)
results = data.frame(pre_process,classification)

counts = matrix(c(pre_process,classification),2,2)
counts = t(counts)
print(counts)

setEPS()
postscript("../../../Report/graphics/compare_timing_knn_smoothVSpca.eps",height = 4, width = 8)
something = barplot(counts, main="KNN Timing",
					ylab="Time [s]", col=colors,
					xlab = "Method Applied",
					ylim=c(0,max(sum(counts[,1]),sum(counts[,2]))),
					legend = colnames(results),
					args.legend = list(x = "topright")
)
axis(1, at=something, labels=c("KNN S+ZS","KNN S+ZS+PCA+ZS"), las = 1)
q = dev.off()
# 


