library("pryr")

source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("C50predict.R")

fileName <- "final_tree_timing.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	data_tree_s = prepareOneAlone(3,2,trainPartSize=400, testSize=400, filter = "gaussian", size = 7, sigma = 0.9, make_new = 1)
	data_tree_c = data_tree_s
	data_tree_s = prepare_data_for_tree(data_tree_s)
	
	pre_process_time_start_tree = proc.time()
	data_tree_c = normalizeData(data_tree_c, "z-score")
	data_tree_c = pca_simplification(data_tree_c,noPC=75)
	pre_process_time_tree = (proc.time() - pre_process_time_start_tree)[["user.self"]]
	
	print("1")
	training_time_start = proc.time()
	model_c = C50::C5.0(data_tree_c$trainSet, as.factor(data_tree_c$trainVali), trials = 24, control = C5.0Control(minCases = 7))
	training_time_tree_c = (proc.time() - training_time_start)[["user.self"]]
	print("2")
	training_time_start = proc.time()
	model_s = C50::C5.0(data_tree_s$trainSet, as.factor(data_tree_s$trainVali), trials = 7, control = C5.0Control(minCases = 7))
	training_time_tree_s = (proc.time() - training_time_start)[["user.self"]]
	
	print("3")
	classification_time_start = proc.time()
	tree_success_c = tree_predict(data=data_tree_c, model=model_c)$success
	classification_time_tree_c = (proc.time() - classification_time_start)[["user.self"]]
	print("4")
	classification_time_start = proc.time()
	tree_success_s = tree_predict(data=data_tree_s, model=model_s)$success
	classification_time_tree_s = (proc.time() - classification_time_start)[["user.self"]]
	
	save(model_s, model_c,training_time_tree_c, training_time_tree_s, pre_process_time_tree, classification_time_tree_c,classification_time_tree_s,tree_success_c, tree_success_s,file=fileName)
}

size_c = object_size(model_c)/(2^20)

size_s = object_size(model_s)/(2^20)

size_data_s = object_size(data_tree_s)/(2^20)
size_data_c = object_size(data_tree_c)/(2^20)


print(paste(c("model s: ", size_s, " MB"),collapse =""))
print(paste(c("model c: ", size_c, " MB"),collapse =""))

print(paste(c("data s: ", size_data_s, " MB"),collapse =""))
print(paste(c("data c: ", size_data_c, " MB"),collapse =""))


colors = rainbow(3);

# knn_time  = c(pre_process_time,0,classification_time_knn)
# tree_time = c(pre_process_time,training_time_tree,classification_time_tree)
pre_process = c(0,pre_process_time_tree)
training = c(training_time_tree_s,training_time_tree_c)
classification = c(classification_time_tree_s,classification_time_tree_c)
results = data.frame(pre_process,training)
# knn_time  = list(pre_process=pre_process_time,training=0,classification=classification_time_knn)
# tree_time  = list(pre_process=pre_process_time,training=training_time_tree,classification=classification_time_tree)
# counts = table(results[1,],results[2,])
counts = matrix(c(pre_process,training),2,2)
counts = t(counts)
print(counts)

setEPS()
postscript("../../../Report/graphics/algo_compare_timing_tree.eps",height = 4, width = 8)
something = barplot(counts, main="Tree Timing",
		ylab="Time [s]", col=colors,
		xlab = "Method Applied",
		ylim=c(0,max(sum(counts[,1]),sum(counts[,2]))),
		legend = colnames(results),
		args.legend = list(x = "topright")
)
axis(1, at=something, labels=c("Tree S","Tree S+ZS+PCA"), las = 1)
q = dev.off()
# 

print(classification_time_tree_c)
print(classification_time_tree_s)


