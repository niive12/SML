# 4h40m with 8 cores 11 PCA's and 21 K's at 400/400
# 7h30m with 8 cores 11 PCA's and 24 K's at 400/400

library("grDevices") # gray.colors
library("parallel") 

source("load_people_data.R")
source("pca_test.R")


makeContours <- function(PCA,k,data,name){
	startTime <- proc.time() # used for timing
	contour_data = matrix(,length(PCA),length(k))
	cl <- makeCluster(getOption("cl.cores", 8))
	
	for(PCA_val in 1:length(PCA)){
		simplifiedData <- pca_simplification(data, breakpoint=PCA[PCA_val])
		contour_data[PCA_val,] <- do.call('c',parLapply(cl,k,function(x){source("pca_test.R");return ((run_knn(data = simplifiedData, K = x))$success)}))
		# 		
#		for(k_val in 1:length(k)){
# 			contour_data[PCA_val,k_val] <- ((run_knn(simplifiedData,k[k_val]))$success)
# 			# timer :D
# 			timer <- ((proc.time() - startTime)[1])
# 			print(paste(c((length(k)*(PCA_val-1) + k_val), "/",(length(PCA)*length(k))," runs completed. Time used till now: ",timer, " seconds."),collapse=""))
# 		}
		print(paste(c(PCA_val, "/",length(PCA)," runs completed."),collapse=""))
	}

	stopCluster(cl)

	# plot the contour plot
	filled.contour(y = k, x = PCA, contour_data, col=colorpanel(10, "white", "black"), levels=seq(min(contour_data), max(contour_data), length.out= 11))
# 	filled.contour(y = k, x = PCA, contour_data, color.palette = heat.colors)
	title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "K")
	setEPS()
	postscript(name,height = 6, width = 8)
	filled.contour(y = k, x = PCA, contour_data, col=colorpanel(10, "white", "black"), levels=seq(min(contour_data), max(contour_data), length.out= 11))
# 	filled.contour(y = k, x = PCA, contour_data, color.palette = heat.colors)
	title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "K")
	dev.off()
	# filled.contour(y = k, x = trainingSetSize, contour_data, col=colorpanel(20, "grey10", "white"), nlevels=20)
	# 
}


# PCA on 1 person only, all-one-vs-all and all-mixed (100 DPI)
# Contour K vs Acc. PC var.
# K: 1:1:20
# PCA: 50:5:100%

testPerson <- c(3,2)


# actual measured points in ascending order
PCA = c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
k = c(seq(1,47,2))
# k = c(seq(1,40,4))

# 
# print("One only")
# data <- prepareAllMixed(360,40,peopleToLoad = list(testPerson))
# makeContours(PCA,k,data,"contour_k_PCA_oneOnly.eps")


# best plot: k = 19, PCA = 0.8
print("One vs Rest")
print(system.time(data <- prepareOneAlone(testPerson[1],testPerson[2], trainPartSize = 400, testSize = 400, peopleToLoad = getPeople())))
print(system.time(makeContours(PCA,k,data,"contour_k_PCA_oneVsRest.eps")))
# 
# print("All mixed")
# data <- prepareAllMixed(100,40,peopleToLoad = getPeople())
# makeContours(PCA,k,data,"contour_k_PCA_allMixed.eps")

