source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("Baye.R")


fileName <- "baye_pre_process.RData"

if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
# 	data = prepareAllMixed(360,40,peopleToLoad = getPeople())
	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )
	print("got data")
# 	data = normalizeData(data, "z-score")
# 	print("z scored")
	data = pca_simplification(data,breakpoint=0.6)
	print("simplified successful")
# 	data = normalizeData(data,"bin", 20)
	print("data is in the bin")
	save(data, file = fileName)
	# 90 sec...
}
# test = 0:5
test = c(0,60000000)
# test = seq(10,20,10)
result = array(0,length(test))
for( i in 1:length(test) ) {
	res = baye_predict(data, laplace=test[i])
	result[i] = res$success
	print(test[i])
	print(res$success)
}

# Hvis tal < eps så sættes tal = threshold
