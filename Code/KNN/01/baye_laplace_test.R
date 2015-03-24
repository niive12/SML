source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("Baye.R")


fileName <- "baye_pre_process.RData"

if ( file.exists(fileName) && 1 ) {
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
	data = normalizeData(data,"bin", 20)
	print("data is in the bin")
	save(data, file = fileName)
	# 90 sec...
}
# test = 0:5
test = seq(0,1,0.3)
# test = seq(10,20,10)
result = array(0,length(test))
for( i in 1:length(test) ) {
	res = baye_predict(data, laplace=3)
	result[i] = res$success
	print(test[i])
	print(res$success)
}
for( i in 1:length(test) ) {
	res = baye_predict(data, laplace=1,threshold=test[i],eps=1)
	result[i] = res$success
	print(test[i])
	print(res$success)
}
for( i in 1:length(test) ) {
	res = baye_predict(data, laplace=test[i],threshold=1,eps=1)
	result[i] = res$success
	print(test[i])
	print(res$success)
}
# 6 min

# Hvis tal < eps så sættes tal = threshold
