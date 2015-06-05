source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("C50predict.R")

fileName <- "tree_vs_knn_time.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )
	pre_process_time_start = proc.time()
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=50)
	pre_process_time = (proc.time() - pre_process_time_start)[["user.self"]]
	
	training_time_start = proc.time()
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali))
	training_time_tree = (proc.time() - training_time_start)[["user.self"]]
	
	classification_time_start = proc.time()
		tree_success = tree_predict(data=data, model=model)$success
	classification_time_tree = (proc.time() - classification_time_start)[["user.self"]]
	
	classification_time_start = proc.time()
		knn_success = run_knn(data=data, K=10)$success
	classification_time_knn = (proc.time() - classification_time_start)[["user.self"]]
	save(data,model,pre_process_time,training_time_tree,classification_time_tree,classification_time_knn,tree_success,knn_success,file=fileName)
}

colors = rainbow(3);


# knn_time  = c(pre_process_time,0,classification_time_knn)
# tree_time = c(pre_process_time,training_time_tree,classification_time_tree)
pre_process = c(pre_process_time,pre_process_time)
training = c(0,training_time_tree)
classification = c(classification_time_knn,classification_time_tree)
results = data.frame(pre_process,training,classification)
# knn_time  = list(pre_process=pre_process_time,training=0,classification=classification_time_knn)
# tree_time  = list(pre_process=pre_process_time,training=training_time_tree,classification=classification_time_tree)
# counts = table(results[1,],results[2,])
counts = matrix(c(pre_process,training,classification),2,3)
counts = t(counts)
print(counts)

setEPS()
postscript("foo.eps",height = 4, width = 8)
barplot(counts, main="knn vs tree timing",
  xlab="time in seconds", col=colors,
  ylim=c(0,150),
  legend = colnames(results),
  args.legend = list(x = "topleft")
  ) 
q = dev.off()
# 
print(c(tree_success,knn_success))