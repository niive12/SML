source("C50predict.R")
source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
# data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )

data = normalizeData(data, "z-score")
data = pca_simplification(data,noPC=50)
model = C50::C5.0(data$trainSet, as.factor(data$trainVali), trials=10)

setEPS()
postscript("../../../Report/graphics/tree_section.eps",height = 4, width = 8)
plot(model,subtree = 24)
q = dev.off()

res = random_forrest_predict(data=data, model=model)
write.latex(res$confusion_matrix, 0:9, 0:9, "../../../Report/graphics/random_forrest_confusion.tex")
print(res$success)