source("load_people_data.R")
source("normalize.R")
source("Baye.R")
library("KernSmooth") 


library("gplots") # contour plot colors

laplace = 0


data <- prepareOneAlone(3,2, 400, 400, peopleToLoad = list(c(3,1),c(3,2)))

startTime <- proc.time() # used for timings

# reduce data (binning and grouping)
data <- normalizeData(data, "bin", bins = 2, inverseBinning = T)

# make dataset to ratio
for(i in 1:dim(data$trainSet)[1]){
	cropCords <- cropDigitLimits(data$trainSet[i,], 20, is2binned = T)	
	
	cropImg <- cropDigit(data$trainSet[i,], 20, cropCords)
	
	ratio <- sum(cropImg)/length(cropImg) # all blacks
	
	data$trainSet[i,] <- ratio
}


for(i in 1:dim(data$testSet)[1]){
	cropCords <- cropDigitLimits(data$testSet[i,], 20, is2binned = T)
	
	cropImg <- cropDigit(data$testSet[i,], 20, cropCords)
	
	ratio <- sum(cropImg)/length(cropImg) # all blacks
	
	data$testSet[i,] <- ratio
}


estimated_pdf = list(c(1:10,1:10))
for( i in 1:10) {
	estimated_pdf[[i]] = bkde(x=(data$trainSet)[((i-1)*(dim(data$trainSet)[1])/10 + 1):(i*(dim(data$trainSet)[1])/10)], kernel="normal")
}

	setEPS()
	postscript("kde_test.eps",height = 6, width = 8)
plot(estimated_pdf[[1]], xlab="x", ylab="pdf", xlim=c(0,1), ylim=c(0,8),type="l",col=colors[1],lty=1) # xlim=c(xmin,xmax), ylim=c(ymin,ymax),
for( i in 2:10) {
	lines(estimated_pdf[[i]],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
}
legend("topright",NULL,0:9,cex=0.8,col=colors,lty=1:10)

	q = dev.off()

# 	for(div_i in 1:length(divs)){
# 		cropCord <- cropDigitLimits(dataRow, charWidth, is2binned = T)
# 		
# 		data_bin <- countBinning(data_b, 20, 0:(bis[bin_i]-1), pictureDevisions = divs[div_i])
# 		succ <- (baye_predict(data_bin, laplace = laplace))$success
# 		contour_data[bin_i,div_i] = succ
# 		print(paste(c((bin_i-1)*length(divs)+div_i, " / ", (length(bis)*length(divs)), " Time taken till now: ",(proc.time()-startTime)[3], " seconds."),collapse=""))
# 	}


