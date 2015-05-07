library("gplots") # contour plot colors

source("load_people_data.R")
source("normalize.R")
source("neural_network_simplified.R")
source("pca_test.R")

fileName <- "nn_contour_pca_vs_size.RData"

makeContour_nn_pca_vs_size <- function(data, time_model, time_predict, result, pc, size, row=1, col=1){	
	startTime = proc.time() # used for timings
	
	for(pc_i in row:length(pc)){
		# simplify data 
		sim_data <- pca_simplification(data, noPC=pc[pc_i])
		for(size_i in col:length(size)){
			# run mlp
			tmp_time_model = proc.time()
			model = neural_network_simplification(sim_data,size=size[size_i])
			time_model[pc_i,size_i] = (proc.time()-tmp_time_model)[["user.self"]]
			
			tmp_time_predict = proc.time()
			result[pc_i,size_i] <- neural_network_predict(model,sim_data)$success
			time_predict[pc_i,size_i] = (proc.time()-tmp_time_predict)[["user.self"]]
			
			print(c(time_model[pc_i,size_i],result[pc_i,size_i],time_predict[pc_i,size_i]))
			
			print(paste(c((pc_i - 1)*length(size) + size_i , "/", length(pc)*length(size) , " datasets loaded. Time taken till now: ",(proc.time()-startTime)[["user.self"]], " seconds."),collapse = ""))
		}
		col = 1
		save(result, time_model, time_predict, file = fileName)
	}
	
	return(list(success=result,time_mod=time_model,time_pre=time_predict))
}

PC = seq(0,300,100)
size <- seq(100,400,100)
PC[1] = 5
reload = 1
if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else if ( file.exists(fileName) && 0 ) {
	print(paste(c("reloaded, test data exists in ", fileName),collapse=""))
	load(fileName)
	
	row = which(is.na(result))[1]
	if( row > 1 ){
		row = row - 1
	}
	col = which(is.na(result[row,]))[1]
	print(c(row,col))
	
	data = prepareOneAlone(3,2, 400, 400)
	data = normalizeData(data, "z-score")
	contour_data = makeContour_nn_pca_vs_size(data, time_model, time_predict, result, PC, size, row, col)
	result  = contour_data$success
	time_model = contour_data$time_mod
	time_predict = contour_data$time_pre
	
} else {
	time_model   = matrix(,length(PC),length(size))
	time_predict = matrix(,length(PC),length(size))
	result       = matrix(,length(PC),length(size))
	
	data = prepareOneAlone(3,2, 400, 400)
	data = normalizeData(data, "z-score")
	contour_data = makeContour_nn_pca_vs_size(data, time_model, time_predict, result, PC, size)
	result  = contour_data$success
	time_model = contour_data$time_mod
	time_predict = contour_data$time_pre
}

plot_name1 = "contour_nn_size_vs_pca.eps"
plot_name2 = "contour_nn_size_vs_pca_time_mod.eps"
plot_name3 = "contour_nn_size_vs_pca_time_pre.eps"

setEPS()
postscript(plot_name1,height = 6, width = 8)
filled.contour(y = size, x = PC,result, col=colorpanel(20, "black", "white"), levels=seq(min(result), max(result), length.out= 21))
title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "No. hidden layers")
dev.off()

setEPS()
postscript(plot_name2,height = 6, width = 8)
filled.contour(y = size, x = PC,time_model, col=colorpanel(20, "black", "white"), levels=seq(min(time_model), max(time_model), length.out= 21))
title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "No. hidden layers")
dev.off()

setEPS()
postscript(plot_name3,height = 6, width = 8)
filled.contour(y = size, x = PC,time_predict, col=colorpanel(20, "black", "white"), levels=seq(min(time_predict), max(time_predict), length.out= 21))
title(main = NULL, xlab = "Accumulated PCA Variance", ylab = "No. hidden layers")
dev.off()
