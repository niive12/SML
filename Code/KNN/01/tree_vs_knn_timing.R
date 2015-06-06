source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("C50predict.R")

fileName <- "tree_vs_knn_time.RData"

if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	data_knn = prepareOneAlone(3,2,trainPartSize=400, testSize=400, filter = "gaussian", size = 7, sigma = 0.9, make_new = 1)
	data_tree = prepareOneAlone(3,2,trainPartSize=400, testSize=400, filter = "gaussian", size = 5, sigma = 0.9, make_new = 1)
	
	pre_process_time_start_knn = proc.time()
		data_knn = normalizeData(data_knn, "z-score")
		data_knn = pca_simplification(data_knn,noPC=40)
		data_knn = normalizeData(data_knn, "z-score")
	pre_process_time_knn = (proc.time() - pre_process_time_start_knn)[["user.self"]]
	
	pre_process_time_start_tree = proc.time()
	data_tree = normalizeData(data_tree, "z-score")
	data_tree = pca_simplification(data_tree,noPC=75)
	pre_process_time_tree = (proc.time() - pre_process_time_start_tree)[["user.self"]]
	
	
	training_time_start = proc.time()
		model = C50::C5.0(data_tree$trainSet, as.factor(data_tree$trainVali), trials = 24, control = C5.0Control(minCases = 7))
	training_time_tree = (proc.time() - training_time_start)[["user.self"]]
	
	classification_time_start = proc.time()
		tree_success = tree_predict(data=data_tree, model=model)$success
	classification_time_tree = (proc.time() - classification_time_start)[["user.self"]]
	
	classification_time_start = proc.time()
		knn_success = run_knn(data=data_knn, K=1)$success
	classification_time_knn = (proc.time() - classification_time_start)[["user.self"]]
	save(data,model,pre_process_time,training_time_tree,classification_time_tree,classification_time_knn,tree_success,knn_success,file=fileName)
}

colors = rainbow(3);


# knn_time  = c(pre_process_time,0,classification_time_knn)
# tree_time = c(pre_process_time,training_time_tree,classification_time_tree)
pre_process = c(pre_process_time_knn,pre_process_time_tree)
training = c(0,training_time_tree)
classification = c(classification_time_knn,classification_time_tree)
results = data.frame(pre_process,classification)
# knn_time  = list(pre_process=pre_process_time,training=0,classification=classification_time_knn)
# tree_time  = list(pre_process=pre_process_time,training=training_time_tree,classification=classification_time_tree)
# counts = table(results[1,],results[2,])
counts = matrix(c(pre_process,classification),2,2)
counts = t(counts)
print(counts)

setEPS()
postscript("../../../Report/graphics/algo_compare_timing.eps",height = 4, width = 8)
barplot(counts, main="KNN vs Tree Timing",
  ylab="Time [s]", col=colors,
  xlab = "Method Applied",
  ylim=c(0,max(sum(counts[1,]),sum(counts[1,]))),
  legend = colnames(results),
  args.legend = list(x = "topright")
  )
axis(1, at=1:2, labels=c("KNN","Tree"), las = 1)
q = dev.off()
# 
print(c(tree_success,knn_success))