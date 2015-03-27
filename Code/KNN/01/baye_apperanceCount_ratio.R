source("load_people_data.R")
source("normalize.R")
source("Baye.R")

library("gplots") # contour plot colors

laplace = 0


if(TRUE){
	data <- prepareOneAlone(3,2, 20, 20)
	
	startTime <- proc.time() # used for timings
	
	# reduce data (binning and grouping)
	data <- normalizeData(data, "bin", bins = 2, inverseBinning = T)
	
	# make dataset to ratio
	for(i in 1:length(data$trainSet)){
		cropCords <- cropDigitLimits(data$trainSet[1,], 20, is2binned = T)
		
		cropImg <- cropDigit(data$trainSet[1,], 20, cropCords)
		
		ratio <- sum(cropImg)/length(cropImg) # all blacks
		
		data$trainSet[i,] <- ratio
	}
	
	for(i in 1:length(data$testSet)){
		cropCords <- cropDigitLimits(data$testSet[1,], 20, is2binned = T)
		
		cropImg <- cropDigit(data$testSet[1,], 20, cropCords)
		
		ratio <- sum(cropImg)/length(cropImg) # all blacks
		
		data$testSet[i,] <- ratio
	}
	
	
	
# 	for(div_i in 1:length(divs)){
# 		cropCord <- cropDigitLimits(dataRow, charWidth, is2binned = T)
# 		
# 		data_bin <- countBinning(data_b, 20, 0:(bis[bin_i]-1), pictureDevisions = divs[div_i])
# 		succ <- (baye_predict(data_bin, laplace = laplace))$success
# 		contour_data[bin_i,div_i] = succ
# 		print(paste(c((bin_i-1)*length(divs)+div_i, " / ", (length(bis)*length(divs)), " Time taken till now: ",(proc.time()-startTime)[3], " seconds."),collapse=""))
# 	}


}else {
}

