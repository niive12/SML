source("load_people_data.R")
source("C50predict.R") # tree_predict
source("pca_test.R")   #pca_simplification
source("normalize.R")  #normalize
fileName <- "tree_raw.RData"


confidence_factor = seq(0.25,1,0.1)
cases = 2:10
test_var = cases

if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	tree_raw = array(0,length(test_var))
# 		data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )
# 		data = prepareAllMixed(50,50, DPI = 100 , filter = "gaussian", peopleToLoad = getPeople(), make_new=1, sigma =0.5, size =5)
	data = prepareOne(group=3, member=2, trainPart=360,testPart=40, DPI = 100 , filter = "gaussian", make_new=1, sigma =0.5, size =5)
# 	data = normalizeData(data, "z-score")
	data = pca_simplification(data,noPC=400)
	for(i in 1:length(test_var) ){

		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
# 		,control=C5.0Control(CF=confidence_factor[i])
		,control=C5.0Control(minCases=cases[i],winnow = TRUE)
# 		,control=C5.0Control(winnow = TRUE)
		)

		tree_raw[i] = tree_predict(data=data, model=model)$success
		print(c(test_var[i], tree_raw[i]))
		save(tree_raw,file=fileName)
	}
}

print(tree_raw)