# library("gplots") # needs to be installe

library("grDevices") # gray.colors

source("load_people_data.R")
source("pca_test.R")


makeContours <- function(PCA,k,data,name){
	startTime <- proc.time() # used for timing
	contour_data = matrix(,length(PCA),length(k))
	for(PCA_val in 1:length(PCA)){
		simplifiedData <- pca_simplification(data, breakpoint=PCA[PCA_val])
		for(k_val in 1:length(k)){
			contour_data[PCA_val,k_val] <- ((run_knn(simplifiedData,k[k_val]))$success)
			# timer :D
			timer <- ((proc.time() - startTime)[1])
			print(paste(c((length(k)*(PCA_val-1) + k_val), "/",(length(PCA)*length(k))," runs completed. Time used till now: ",timer, " seconds."),collapse=""))
		}
	}
	
	# plot the contour plot
	filled.contour(y = k, x = PCA, contour_data, color.palette = gray.colors)
# 	filled.contour(y = k, x = PCA, contour_data, color.palette = heat.colors)
	title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "K")
	setEPS()
	postscript(name,height = 6, width = 8)
	filled.contour(y = k, x = PCA, contour_data, color.palette = gray.colors)
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
k = 1:20
# k = c(seq(1,40,4))


print("One only")
data <- prepareAllMixed(360,40,peopleToLoad = list(testPerson))
makeContours(PCA,k,data,"contour_k_PCA_oneOnly.eps")


print("One vs Rest")
data <- prepareOneAlone(testPerson[1],testPerson[2], trainPartSize = 100, testSize = 40, peopleToLoad = getPeople())
makeContours(PCA,k,data,"contour_k_PCA_oneVsRest.eps")


print("All mixed")
data <- prepareAllMixed(100,40,peopleToLoad = getPeople())
makeContours(PCA,k,data,"contour_k_PCA_allMixed.eps")



