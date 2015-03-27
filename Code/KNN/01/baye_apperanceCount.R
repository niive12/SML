source("load_people_data.R")
source("normalize.R")
source("Baye.R")

library("gplots") # contour plot colors

divs = 1:10
bis = 2:10

laplace = 0

contour_data <- matrix(,length(bis),length(divs))

if(FALSE){
	data <- prepareOneAlone(3,2, 400, 400)
	
	startTime <- proc.time() # used for timings
	
	# reduce data (binning and grouping)
	for(bin_i in 1:length(bis)){
		data_b <- normalizeData(data, "bin", bins = bis[bin_i])
		for(div_i in 1:length(divs)){
			data_bin <- countBinning(data_b, 20, 0:(bis[bin_i]-1), pictureDevisions = divs[div_i])
			succ <- (baye_predict(data_bin, laplace = laplace))$success
			contour_data[bin_i,div_i] = succ
			print(paste(c((bin_i-1)*length(divs)+div_i, " / ", (length(bis)*length(divs)), " Time taken till now: ",(proc.time()-startTime)[3], " seconds."),collapse=""))
		}
	}
	
	save(contour_data, file = "contour_baye_bin-vs-divs.Rdata")
	
}else {
	load("contour_baye_bin-vs-divs.Rdata")
}

filled.contour(y = divs, x = bis, contour_data, col=colorpanel(20, "black", "white"), levels=seq(min(contour_data), max(contour_data), length.out= 21))
title(main = NULL, xlab = "No. of bins", ylab = "No. of divisions")

setEPS()
postscript("contour_bins_vs_divs.eps",height = 6, width = 8)
filled.contour(y = divs, x = bis, contour_data, col=colorpanel(20, "black", "white"), levels=seq(min(contour_data), max(contour_data), length.out= 21))
title(main = NULL, xlab = "No. of bins", ylab = "No. of divisions")
dev.off()