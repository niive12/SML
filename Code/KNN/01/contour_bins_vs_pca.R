# 2h40m for PC <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1) bins <- 1:10
# 6h for PC <- c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1) bins <- 1:20


library("gplots") # contour plot colors

source("load_people_data.R")
source("normalize.R")
source("Baye.R")
source("pca_test.R")


makeContour_bin_pca <- function(data, pc, bins){
	result <- matrix(,length(pc),length(bins))
	
	startTime <- proc.time() # used for timings
	
	for(pc_i in 1:length(pc)){
		# simplify data 
		simplifiedData <- pca_simplification(data, breakpoint=pc[pc_i])
		for(bin_i in 1:length(bins)){
			# binarize data
			data_norm = normalizeData(simplifiedData,"bin", bins = bins[bin_i])
			# run bayes
			baye_pre = baye_predict(data_norm, laplace = 1)
			# save data
			result[pc_i,bin_i] <- baye_pre$success
			
			print(paste(c((pc_i - 1)*length(bins) + bin_i , "/", length(pc)*length(bins) , " datasets loaded. Time taken till now: ",(proc.time()-startTime)[3], " seconds."),collapse = ""))
		}
	}
	
	return(result)
}



testPerson <- c(3,2)
PC <- c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
bins <- seq(2,200,10)

if(TRUE){
	data <- prepareOneAlone(testPerson[1],testPerson[2], trainPartSize = 400, testSize = 400, peopleToLoad = getPeople())
	data <- normalizeData(data,"z-score")
	contour_data <- makeContour_bin_pca(data, PC, bins)
	save(contour_data, file = "contour_bin-vs-pca.Rdata")
}else {
	load("contour_bin-vs-pca.Rdata")
}

# plot the contour plot
filled.contour(y = bins, x = PC, contour_data, col=colorpanel(20, "black", "white"), levels=seq(min(contour_data), max(contour_data), length.out= 21))
title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "No. of bins")

setEPS()
postscript("contour_bins_vs_pca.eps",height = 6, width = 8)
filled.contour(y = bins, x = PC,contour_data, col=colorpanel(20, "black", "white"), levels=seq(min(contour_data), max(contour_data), length.out= 21))
title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "No. of bins")
dev.off()

