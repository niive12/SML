source("load_people_data.R")
source("C50predict.R") # tree_predict
source("pca_test.R")   #pca_simplification
source("normalize.R")  #normalize
library("gplots")
library("graphics")
source("confusion_matrix.R")

new_raw              = 0
new_smooth           = 0 #add labels
new_pca              = 0 #without smoothing
new_total            = 0
new_t_mix            = 0
new_t_all            = 0
new_pca_vs_boost     = 1
new_performance_mix  = 0
new_performance_all  = 0
new_performance_mix2 = 0
new_performance_all2 = 0

colors = rainbow(6)

if( new_raw == 1 ){
	fileName <- "tree_raw.RData"

	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		cases = 2:10
		test_var = cases
		tree_raw = array(0,length(test_var))
		data = prepareOne(group=3, member=2, trainPart=360,testPart=40, DPI = 100 , filter = "none", make_new=1, sigma =0.5, size =5)
	# 	data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=400)
		for(i in 1:length(test_var) ){

			model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
	# 		,control=C5.0Control(CF=confidence_factor[i])
			,control=C5.0Control(minCases=cases[i],winnow = TRUE)
	# 		,control=C5.0Control(winnow = TRUE)
			)

			tree_raw[i] = tree_predict(data=data, model=model)$success
			print(c(test_var[i], tree_raw[i]))
			save(cases,tree_raw,file=fileName)
		}
	}

	setEPS()
	postscript("../../../Report/graphics/tree_raw.eps",height = 4, width = 8)
	plot(cases, tree_raw, pch=16, xlab="Minimum Cases",ylab="Success Rate", ylim=c(min(tree_raw),max(tree_raw)), col=colors[1], type="o")
	q = dev.off()
}
best_min_cases = 7
if( new_smooth == 1){
	fileName <- "tree_smooth.RData"

	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		sigma = seq(0.1,2,0.1)
		k_size = seq(3,15,2)
		tree_smooth = matrix(0,length(sigma),length(k_size))
		for(smooth_sigma in 1:length(sigma) ){
			for(kernel_size in 1:length(k_size) ){
				data = prepareOne(group=3, member=2, trainPart=360,testPart=40, DPI = 100 , filter = "gaussian", make_new=1, sigma =sigma[smooth_sigma], size =size[kernel_size])
				data = pca_simplification(data,noPC=400)

				model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
				,control=C5.0Control(minCases=best_min_cases)
				)

				print(tree_smooth)
				tree_smooth[smooth_sigma,kernel_size] = tree_predict(data=data, model=model)$success
				save(sigma,k_size,tree_smooth,file=fileName)
			}
		}
	}
	# find max
	point = which.max(t(tree_smooth))
	x_p = point %% length(k_size)
	y_p = ceiling(point/length(k_size))
# 	print(c(point,x_p,y_p,dim(tree_smooth),sigma[y_p],k_size[x_p]))
# print(t(tree_smooth))	

	setEPS()
	postscript("../../../Report/graphics/tree_smooth.eps",height = 6, width = 8)
	filled.contour(y = sigma, x = k_size, t(tree_smooth), col=colorpanel(20, "black", "white"), 
					levels=seq(min(tree_smooth), max(tree_smooth), length.out= 21), 
					xlab="Kernel Size",ylab="Deviance",
					locator={points(y = sigma[y_p], x = k_size[x_p], col = "red")}
	)
	text(size[x_p],sigma[y_p],tree_smooth[point])
	q = dev.off()
}
best_kernel_size = 7
best_sigma       = 0.9
if( new_pca == 1){
	fileName <- "tree_pca.RData"

	if ( file.exists(fileName) && 0 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		PC = seq(5,350,5)
		tree_PCA = array(0,length(PC))
		for(n in 1:length(PC) ){
			data = prepareOne(group=3, member=2, trainPart=360,testPart=40, DPI = 100 , filter = "none", make_new=1, sigma =best_sigma, size =best_kernel_size)
			data = pca_simplification(data,noPC=PC[n])
			
			model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
			,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))

			tree_PCA[n] = tree_predict(data=data, model=model)$success
			save(tree_PCA,PC,file=fileName)
		}
	}
	setEPS()
	postscript("../../../Report/graphics/tree_pca.eps",height = 6, width = 8)
	plot(PC, tree_PCA, pch=16, xlab="No of PC",ylab="Success Rate", ylim=c(min(tree_PCA),max(tree_PCA)), col=colors[1], type="o")
	q = dev.off()
}
best_PC = 130
if( new_total == 1){
	fileName <- "tree_total.RData"
	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		order = c("Raw", "ZS", "PCA", "PCA+ZS", "ZS+PCA", "ZS+PCA+ZS", "S", "S+ZS", "S+PCA", "S+PCA+ZS", "S+ZS+PCA", "S+ZS+PCA+ZS")
		result = array(0,length(order))
		
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[1] = tree_predict(data=data, model=model)$success
	print(result)
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[2] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[3] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C5.0(data$trainSet, as.factor(data$trainVali),control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[4] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[5] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[6] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[7] = tree_predict(data=data, model=model)$success
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[8] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[9] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[10] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[11] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[12] = tree_predict(data=data, model=model)$success
	print(result)

		save(result,file=fileName)
	}
	setEPS()
	postscript("../../../Report/graphics/tree_total.eps",height = 5, width = 8)
	par(mar=c(7,4,4,0))
	ylimits = c(round((2 * min(result) - max(result) - 0.05), digits = 1), round((max(result) + 0.05), digits = 1))
	if(ylimits[1] < 0){ylimits[1] = 0}
	barplot(rep(NA,length(result)),ylim=ylimits,axes=FALSE)
	barplot(result, ylab = "Success", names.arg=order, horiz=F,las=2, ylim=ylimits, add = T, xpd = F)
	abline(h=mean(result) ,ylim=ylimits)
	q = dev.off()
}
if( new_t_mix == 1){
	fileName <- "tree_total_mixed.RData"
	if ( file.exists(fileName) && 0 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		order = c("Raw", "ZS", "PCA", "PCA+ZS", "ZS+PCA", "ZS+PCA+ZS", "S", "S+ZS", "S+PCA", "S+PCA+ZS", "S+ZS+PCA", "S+ZS+PCA+ZS")
		result = array(0,length(order))
		
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[1] = tree_predict(data=data, model=model)$success
	print(result)
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[2] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[3] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C5.0(data$trainSet, as.factor(data$trainVali),control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[4] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[5] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="none", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[6] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[7] = tree_predict(data=data, model=model)$success
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[8] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[9] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[10] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[11] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareAllMixed(100,50,DPI=100,filter="gaussian", make_new=1,sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[12] = tree_predict(data=data, model=model)$success
	print(result)

		save(result,file=fileName)
	}
	setEPS()
	postscript("../../../Report/graphics/tree_total_mixed.eps",height = 5, width = 8)
	par(mar=c(7,4,4,0))
	ylimits = c(round((2 * min(result) - max(result) - 0.05), digits = 1), round((max(result) + 0.05), digits = 1))
	if(ylimits[1] < 0){ylimits[1] = 0}
	barplot(rep(NA,length(result)),ylim=ylimits,axes=FALSE)
	barplot(result, ylab = "Success", names.arg=order, horiz=F,las=2, ylim=ylimits, add = T, xpd = F)
	abline(h=mean(result) ,ylim=ylimits)
	q = dev.off()
}
if( new_t_all == 1){
	fileName <- "tree_total_all.RData"
	if ( file.exists(fileName) && 0 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		order = c("Raw", "ZS", "PCA", "PCA+ZS", "ZS+PCA", "ZS+PCA+ZS", "S", "S+ZS", "S+PCA", "S+PCA+ZS", "S+ZS+PCA", "S+ZS+PCA+ZS")
		result = array(0,length(order))
		
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[1] = tree_predict(data=data, model=model)$success
	print(result)
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[2] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[3] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C5.0(data$trainSet, as.factor(data$trainVali),control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[4] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[5] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="none", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[6] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[7] = tree_predict(data=data, model=model)$success
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[8] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[9] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[10] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[11] = tree_predict(data=data, model=model)$success
	print(result)
		
		data = prepareOneAlone(3,2,50,100, filter="gaussian", make_new=1, sigma=best_sigma, size=best_kernel_size)
		data = normalizeData(data, "z-score")
		data = pca_simplification(data,noPC=best_PC)
		data = normalizeData(data, "z-score")
		data = prepare_data_for_tree(data)
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali)
		                 ,control=C5.0Control(minCases=best_min_cases,winnow = TRUE))
		result[12] = tree_predict(data=data, model=model)$success
	print(result)

		save(result,file=fileName)
	}
	setEPS()
	postscript("../../../Report/graphics/tree_total_all.eps",height = 5, width = 8)
	par(mar=c(7,4,4,0))
	ylimits = c(round((2 * min(result) - max(result) - 0.05), digits = 1), round((max(result) + 0.05), digits = 1))
	if(ylimits[1] < 0){ylimits[1] = 0}
	barplot(rep(NA,length(result)),ylim=ylimits,axes=FALSE)
	barplot(result, ylab = "Success", names.arg=order, horiz=F,las=2, ylim=ylimits, add = T, xpd = F)
	abline(h=mean(result) ,ylim=ylimits)
	q = dev.off()
}
if( new_pca_vs_boost == 1){
	fileName <- "tree_pca_vs_boost.RData"

	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		pca = c(10,seq(20,400,20))
		trials = seq(1,30,2)
		time_pca_boost    = matrix(0,length(pca),length(trials))
		success_pca_boost = matrix(0,length(pca),length(trials))
		if(file.exists(fileName)){
			load(fileName)
		}
		
		static_data = prepareOne(3,2,360,40, DPI = 100 , filter = "gaussian", make_new=1, sigma =best_sigma, size=best_kernel_size)
# 		static_data = normalizeData(static_data, "z-score")
		data.pca = prcomp(na.pass(static_data$trainSet), center=TRUE, scale=FALSE)
		
		for(n in length(pca)-1:length(pca) ){
			data = subset_pca(static_data,data.pca,noPC=pca[n])
			for(i in 1:length(trials) ){
				
				tmp_time_predict = proc.time()
				model = C50::C5.0(data$trainSet, as.factor(data$trainVali),trials=trials[i]
				,control=C5.0Control(minCases=best_min_cases)
				)
				time_pca_boost[n,i] = (proc.time()-tmp_time_predict)[["user.self"]]
				
				success_pca_boost[n,i] = tree_predict(data=data, model=model)$success
				print(c(success_pca_boost[n,i],time_pca_boost[n,i],((n-1)*length(trials)+i)/(length(trials)*length(pca))))
				save(pca,trials,success_pca_boost,time_pca_boost,file=fileName)
			}
		}
	}
	# find max
	new_success = matrix(0,length(pca),length(trials))
	new_success[5:10,1:13] = success_pca_boost[5:10,1:13]
	point = which.max(new_success)
	x_p = point %% length(pca)
	y_p = ceiling(point/length(pca))
	
	print(c(point,x_p,y_p,success_pca_boost[point]))
	
	setEPS()
	postscript("../../../Report/graphics/tree_pca_vs_boost_success.eps",height = 6, width = 8)
	filled.contour(y = trials, x = pca,success_pca_boost, col=colorpanel(20, "black", "white"), levels=seq(min(success_pca_boost), max(success_pca_boost), length.out= 21),
	locator={points(y = trials[y_p], x = pca[x_p], col = "red")},
	xlab="PC",ylab="Trials"
	)
	text(pca[x_p],trials[y_p],success_pca_boost[point])
	q = dev.off()
	
	setEPS()
	postscript("../../../Report/graphics/tree_pca_vs_boost_time.eps",height = 6, width = 8)
	filled.contour(y = trials, x = pca,time_pca_boost, col=colorpanel(20, "white", "black"), levels=seq(min(time_pca_boost), max(time_pca_boost), length.out= 21),
	xlab="PC",ylab="Trials"
	)
	q = dev.off()
}

best_trials = 24
best_PC     = 75
if( new_performance_all == 1){
	fileName <- "tree_performance_all.RData"
	if ( file.exists(fileName) && 0 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		people = getPeople()
		confus = matrix(0,10,10)
		success = array(0,length(people))
		
		x_lab <- 1:length(people)
		for(i in 1:length(people) ){
			x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
		}
		
		load(fileName)
		for(person in 13:length(people) ){
			data = prepareOneAlone(people[[person]][1], people[[person]][2],400,400, DPI = 100 , filter = "gaussian", make_new=0, sigma =best_sigma, size=best_kernel_size)
			data = normalizeData(data, "z-score")
			data = pca_simplification(data,noPC=best_PC)
			data = prepare_data_for_tree(data)
			
			model = C50::C5.0(data$trainSet, as.factor(data$trainVali),trials=best_trials
			,control=C5.0Control(minCases=best_min_cases)
			)
			
			result = tree_predict(data=data, model=model)
			success[person] = result$success
			confus = confus + result$confusion_matrix
			save(x_lab,success,confus,people,file=fileName)
			
			setEPS()
			postscript("../../../Report/graphics/tree_performance_all.eps",height = 6, width = 8)
			plot(1:length(people),success, xaxt="n",type="b",xlab="Person",ylab="Success Rate") 
			abline(h=mean(success), col = "red")
			axis(1, at=1:length(people), labels=x_lab, las=2)
			dev.off()
		}
	}
	x_lab <- 1:length(people)
	for(i in 1:length(people) ){
		x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
	}
	
	setEPS()
	postscript("../../../Report/graphics/tree_performance_all.eps",height = 6, width = 8)
	plot(1:length(people),success, xaxt="n",type="b",xlab="Person",ylab="Success Rate") 
	abline(h=mean(success), col = "red")
	axis(1, at=1:length(people), labels=x_lab, las=2)
	dev.off()
	
	confusion_matrix(confus, filename="../../../Report/graphics/tree_confusion_all.eps")
	print(paste(c("Mean / Var of the hard: ", mean(success), " / ", var(success)), collapse = ""))
}

if( new_performance_mix == 1){
	fileName <- "tree_performance_mix.RData"
	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		people = getPeople()
		confus = matrix(0,10,10)
		success = array(0,length(people))
		load("crossVal_DPI100_0.9_FILTERgaussian_10.RData")
		
# 		finalData = prepareAllMixedCrossVal(split = 0.9, crossValRuns = 10, filter = "gaussian", size = best_kernel_size, sigma = best_sigma, make_new = 1, peopleToLoad = people)
		for(cross_validation in 1:10 ){
			data = normalizeData(finalData[[cross_validation]], normMethod = "z-score")
			data = pca_simplification(data,noPC=best_PC)
			data = prepare_data_for_tree(data)
			
			model = C50::C5.0(data$trainSet, as.factor(data$trainVali),trials=best_trials
			,control=C5.0Control(minCases=best_min_cases)
			)
			
			result = tree_predict(data=data, model=model)
			success[cross_validation] = result$success
			confus = confus + result$confusion_matrix
			save(success,confus,people,file=fileName)
			
			setEPS()
			postscript("../../../Report/graphics/tree_performance_mix.eps",height = 4, width = 8)
			par(mar=c(1, 4, 4, 1) + 0.1)
			boxplot(success,ylab="Success Rate", outline = FALSE) 
			dev.off()
		}
	}
	
	setEPS()
	postscript("../../../Report/graphics/tree_performance_mix.eps",height = 4, width = 8)
	par(mar=c(1, 4, 4, 1) + 0.1)
	boxplot(success,ylab="Success Rate", outline = FALSE) 
	dev.off()

	confusion_matrix(confus, filename="../../../Report/graphics/tree_confusion_mix.eps")
	print(paste(c("Mean / Var of the easy: ", mean(success), " / ", var(success)), collapse = ""))
}

best_trials = 7
if( new_performance_all2 == 1){
	fileName <- "tree_performance_all2.RData"

	
	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		people = getPeople()
		confus = matrix(0,10,10)
		success = array(0,length(people))
		
		x_lab <- 1:length(people)
		for(i in 1:length(people) ){
			x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
		}

		for(person in length(people):10 ){
			data = prepareOneAlone(people[[person]][1], people[[person]][2],400,400, DPI = 100 , filter = "gaussian", make_new=0, sigma =best_sigma, size=best_kernel_size)
# 			data = normalizeData(data, "z-score")
# 			data = pca_simplification(data,noPC=best_PC)
			data = prepare_data_for_tree(data)
			
			model = C50::C5.0(data$trainSet, as.factor(data$trainVali), trials=best_trials,control=C5.0Control(minCases=best_min_cases))
			
			result = tree_predict(data=data, model=model)
			success[person] = result$success
			confus = confus + result$confusion_matrix
			save(x_lab, success,confus,people,file=fileName)
			
			setEPS()
			postscript("../../../Report/graphics/tree_performance_all2.eps",height = 6, width = 8)
			plot(1:length(people),success, xaxt="n",type="b",xlab="Person",ylab="Success Rate") 
			abline(h=mean(success), col = "red")
			axis(1, at=1:length(people), labels=x_lab, las=2)
			dev.off()
		}
	print(success)
	}
	
	setEPS()
	postscript("../../../Report/graphics/tree_performance_all2.eps",height = 6, width = 8)
	plot(1:length(people),success, xaxt="n",type="b",xlab="Person",ylab="Success Rate") 
	abline(h=mean(success), col = "red")
	axis(1, at=1:length(people), labels=x_lab, las=2)
	dev.off()
	
	confusion_matrix(confus, filename="../../../Report/graphics/tree_confusion_all2.eps")
	print(paste(c("Mean / Var of the hard: ", mean(success), " / ", var(success)), collapse = ""))
}

if( new_performance_mix2 == 1){
	fileName <- "tree_performance_mix2.RData"
	if ( file.exists(fileName) && 1 ) {
		print(paste(c("test data exists in ", fileName),collapse=""))
		load(fileName)
	} else {
		people = getPeople()
		confus = matrix(0,10,10)
		success = array(0,length(people))
		load("crossVal_DPI100_0.9_FILTERgaussian_10.RData")
		
# 		finalData = prepareAllMixedCrossVal(split = 0.9, crossValRuns = 10, filter = "gaussian", size = best_kernel_size, sigma = best_sigma, make_new = 1, peopleToLoad = people)
		load(fileName)
		for(cross_validation in 1:10 ){
			data = normalizeData(finalData[[cross_validation]], normMethod = "z-score")
			data = pca_simplification(data,noPC=best_PC)
			data = prepare_data_for_tree(data)
			
			model = C50::C5.0(data$trainSet, as.factor(data$trainVali),trials=best_trials
			,control=C5.0Control(minCases=best_min_cases)
			)
			
			result = tree_predict(data=data, model=model)
			success[cross_validation] = result$success
			confus = confus + result$confusion_matrix
			save(success,confus,people,file=fileName)
			
			setEPS()
			postscript("../../../Report/graphics/tree_performance_mix2.eps",height = 4, width = 8)
			par(mar=c(1, 4, 4, 1) + 0.1)
			boxplot(success,ylab="Success Rate", outline = FALSE) 
			dev.off()
			print(success)
			rm(data)
			rm(model)
		}
	}
	print(success)
	
	setEPS()
	postscript("../../../Report/graphics/tree_performance_mix2.eps",height = 4, width = 8)
	par(mar=c(1, 4, 4, 1) + 0.1)
	boxplot(success,ylab="Success Rate", outline = FALSE) 
	dev.off()

	confusion_matrix(confus, filename="../../../Report/graphics/tree_confusion_mix2.eps")
	print(paste(c("Mean / Var of the easy: ", mean(success), " / ", var(success)), collapse = ""))
}

