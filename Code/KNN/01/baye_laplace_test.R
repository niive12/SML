source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("Baye.R")


fileName <- "baye_pre_process.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	data = prepareAllMixed(360,40,peopleToLoad = getPeople())
	data = normalizeData(data, "z-score")
	data = pca_simplification(data,breakpoint=0.7)
	data = normalizeData(data,"bin", 20)
	save(data, file = fileName)
	# 90 sec...
}
# test = 0:5
# test = seq(0,2,0.1)
test = seq(0,200,10)
result = array(0,length(test))
for( i in 1:length(test) ) {
	print(c(i, test[[i]]))
	res = baye_predict(data, laplace=test[i])
	result[i] = res$success
	print(res$success)
}
# 6 min