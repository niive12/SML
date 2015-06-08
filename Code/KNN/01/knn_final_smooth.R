# test for the final report
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")


testSetSize = 40
trainSetSize = 360


# smoothing test
if(F){
	s_k = 1
	data_sigma = c(seq(0.1,2,0.1))
	data_size = c(seq(3,15,2))
	smooth_knn_success = matrix(0,length(data_size),length(data_sigma))
	
	for(f_size in 1:length(data_size)){
		for(f_sigma in 1:length(data_sigma)){
			print(paste(c("Running filter contour: ", (f_size - 1) * length(data_sigma) + f_sigma, " / ", length(data_size)*length(data_sigma)), collapse = ""))
			data = prepareOne(3, 2, trainSetSize, testSetSize, filter = "gaussian", sigma = data_sigma[f_sigma], size = data_size[f_size], make_new = 1)
			
			knn_res = run_knn(data, s_k)
			
			smooth_knn_success[f_size, f_sigma] = knn_res$success
			
			setEPS()
			postscript("../../../Report/graphics/knn_smooth_cont.eps",height = 4, width = 8)
			
			point = which.max(smooth_knn_success)
			x_p = point %% length(data_size)
			y_p = ceiling(point/length(data_size))
			
			filled.contour(y = data_sigma, x = data_size, smooth_knn_success, 
						   col=colorpanel(20, "black", "white"), 
						   levels=seq(min(smooth_knn_success), max(smooth_knn_success), length.out= 21), 
						   locator={points(x = data_size[x_p],y = data_sigma[y_p], col = "red")})
			title(main = NULL, xlab = "Filter Size", ylab = "Sigma")
			quiet = dev.off()

			save(smooth_knn_success, data_sigma, data_size, file = "KNN_final_smooth.RData")
		}
	}
}else{
	load("KNN_final_smooth.RData")
}


setEPS()
postscript("../../../Report/graphics/knn_smooth_cont.eps",height = 4, width = 8)
			
point = which.max(smooth_knn_success)
x_p = point %% length(data_size)
y_p = ceiling(point/length(data_size))
			
filled.contour(y = data_sigma, x = data_size, smooth_knn_success,
			   col=colorpanel(20, "black", "white"), levels=seq(min(smooth_knn_success), max(smooth_knn_success), length.out= 21), 
			   locator={
			   	points(x = data_size[x_p],y = data_sigma[y_p], col = "red"); 
			   	text(data_size[x_p], data_sigma[y_p],round(smooth_knn_success[point],2), pos = 4)
			   }
			   )
title(main = NULL, xlab = "Filter Size", ylab = "Sigma")
quiet = dev.off()


