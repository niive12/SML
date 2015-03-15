library("graphics")
library("stats")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

# compare KNN and K-means in table = performance and runtime # Lukas kmean = 400

# kmean = 1500
kmean = 1300
k_knn = 10
k_mean = 10
kmean_iterations = 10


# kmean = 1300
# k_knn = 10
# k_mean = 10
# kmean_iterations = 500
# Time taken to prep kmean: 675.6"
# [1] "Time taken to run kmean classification: 37.22"
# [1] "Success for kmean: 0.53325"
# [1] "Time taken to prep norm kmean: 737.88"
# [1] "Time taken to run norm kmean classification: 37.21"
# [1] "Success for norm kmean: 0.6045"
# [1] "Time taken to run raw knn classification: 1849.95"
# [1] "Success for raw knn: 0.742"
# kmean_iterations = 10
# Time taken to prep kmean: 637.23"
# [1] "Time taken to run kmean classification: 37.4300000000003"
# [1] "Success for kmean: 0.5185"
# [1] "Time taken to prep norm kmean: 719.02"
# [1] "Time taken to run norm kmean classification: 37.5200000000004"
# [1] "Success for norm kmean: 0.61125"



# kmean = 1500
# k_knn = 10
# k_mean = 10
# kmean_iterations = 500
# Time taken to prep kmean: 747.229999999996
# Time taken to run kmean classification: 42.1600000000035
# Success for kmean: 0.54275
# Time taken to prep norm kmean: 802.549999999996
# Time taken to run norm kmean classification: 44.4400000000023
# Success for norm kmean: 0.63125
# Time taken to run raw knn classification: 1878.1
# Success for raw knn: 0.73625


# kmean = 1500
# k_knn = 10
# k_mean = 10
# kmean_iterations = 500
# Time taken to prep kmean: 692.27"
# [1] "Time taken to run kmean classification: 42.6500000000015"
# [1] "Success for kmean: 0.536"
# [1] "Time taken to run raw knn classification: 1839.56"
# [1] "Success for raw knn: 0.7395"

# kmean = 400
# k_knn = 10
# k_mean = 10
# Time taken to prep kmean: 245.92"
# [1] "Time taken to run kmean classification: 8.88000000000011"
# [1] "Success for kmean: 0.382"
# [1] "Time taken to run raw knn classification: 1962.35"
# [1] "Success for raw knn: 0.73975"
# 
# kmean_iterations = 500
# Time taken to prep kmean: 272.61"
# [1] "Time taken to run kmean classification: 8.47000000000003"
# [1] "Success for kmean: 0.4415"
# [1] "Time taken to run raw knn classification: 1857.24"
# [1] "Success for raw knn: 0.739"

# kmean = 400
# k_knn = 10
# k_mean = 1
# kmean_iterations = 500
# Time taken to prep kmean: 240.11"
# [1] "Time taken to run kmean classification: 8.26000000000022"
# [1] "Success for kmean: 0.4305"
# [1] "Time taken to run raw knn classification: 1869.62"
# [1] "Success for raw knn: 0.73775"

# kmean = 400
# k_knn = 10
# k_mean = 1
# Time taken to prep kmean: 279.69
# Time taken to run kmean classification: 8.86000000000058
# Success for kmean: 0.4335
# Time taken to run raw knn classification: 1959.45"


people <- getPeople()
noPeople <- length(people)

# make unique filename
fileName <- "kmean-test"

for(i in 1:noPeople){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")



# run test
if(file.exists(fileName) && 0){
	load(fileName)
} else{
	data <- prepareOneAlone(3,2, trainPartSize = 400, testSize = 400, peopleToLoad = people)
	
	# pure k means 
	startTimeKmeanPrep <- proc.time()
	dataKMeans = kmeans(data$trainSet, kmean, iter.max = kmean_iterations)
	clusterClasifications = 1:kmean
	
	for(cluster_i in 1:kmean) {
		digit = array(0,10);
		for(entry_i in 1:length(dataKMeans$cluster)){
			if (dataKMeans$cluster[entry_i] == cluster_i ){
				digit[data$trainVali[entry_i]] = digit[data$trainVali[entry_i]] + 1;
			}
		}
		# find most present digit
		class = 1
		for(d in 2:10){
			if(digit[d] > digit[class]){
				class = d;
			}
		}
		# update cluster classification
		clusterClasifications[cluster_i] = class
	}
	
	kmeanPrepTime <- (proc.time() - startTimeKmeanPrep)[3]
	print(paste(c("Time taken to prep kmean: ",kmeanPrepTime),collapse=""))
	# run knn on data
	kmeanData = list(trainSet = dataKMeans$centers,trainVali = clusterClasifications,testSet = data$testSet,testVali = data$testVali)

	startTimeKmean <- proc.time()
	kmeanResult = run_knn(kmeanData, k_mean)

	kmeanRunTime <- (proc.time() - startTimeKmean)[3]
	print(paste(c("Time taken to run kmean classification: ",kmeanRunTime),collapse=""))
	print(paste(c("Success for kmean: ",kmeanResult$success),collapse=""))
	
	# noralized k means 
	startTimeKmeanPrep <- proc.time()
	data_norm <- normalizeData(data, "z-score")
	dataKMeans_norm = kmeans(data_norm$trainSet, kmean, iter.max = kmean_iterations)
	clusterClasifications = 1:kmean
	
	for(cluster_i in 1:kmean) {
		digit = array(0,10);
		for(entry_i in 1:length(dataKMeans_norm$cluster)){
			if (dataKMeans_norm$cluster[entry_i] == cluster_i ){
				digit[data_norm$trainVali[entry_i]] = digit[data_norm$trainVali[entry_i]] + 1;
			}
		}
		# find most present digit
		class = 1
		for(d in 2:10){
			if(digit[d] > digit[class]){
				class = d;
			}
		}
		# update cluster classification
		clusterClasifications[cluster_i] = class
	}
	
	kmeanPrepTime <- (proc.time() - startTimeKmeanPrep)[3]
	print(paste(c("Time taken to prep norm kmean: ",kmeanPrepTime),collapse=""))
	# run knn on data
	kmeanData_norm = list(trainSet = dataKMeans_norm$centers,trainVali = clusterClasifications,testSet = data_norm$testSet,testVali = data_norm$testVali)
	
	startTimeKmean <- proc.time()
	kmeanResult_norm = run_knn(kmeanData_norm, k_mean)
	
	kmeanRunTime_norm <- (proc.time() - startTimeKmean)[3]
	print(paste(c("Time taken to run norm kmean classification: ",kmeanRunTime_norm),collapse=""))
	print(paste(c("Success for norm kmean: ",kmeanResult_norm$success),collapse=""))
	
	
	# run pure knn
	startTimeKNN <- proc.time()
	knnResult = run_knn(data,k_knn)
	knnRunTime <- (proc.time() - startTimeKNN)[3]
	print(paste(c("Time taken to run raw knn classification: ",knnRunTime),collapse=""))
	print(paste(c("Success for raw knn: ",knnResult$success),collapse=""))
	
}

