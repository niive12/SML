source("C50predict.R")
source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
library("tree")

fileName <- "tree_plot.Rdata"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {

	# data = prepareOne(group=3,member=2,trainPart=360,testPart=40)
	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )

	data = normalizeData(data, "z-score")
	data = pca_simplification(data,noPC=50)
	model = C50::C5.0(data$trainSet, as.factor(data$trainVali))

	# trainClassF ~ ., data = as.data.frame(trainingMod)
	# model = tree(data$trainVali ~ ., as.data.frame(data$trainSet) )
	# model = C5.0(data$trainVali, as.data.frame(data$trainSet) )
# 	summary(model)

	save(model, data, file=fileName)
}
print("loaded")
setEPS()
postscript("../../../Report/graphics/tree_section.eps",height = 4, width = 8)
plot(model,subtree = 18)
q = dev.off()
print("plottet")

res = random_forrest_predict(data=data, model=model)
write.latex(res$confusion_matrix, 0:9, 0:9, "../../../Report/graphics/random_forrest_confusion.tex")
print(res$success)