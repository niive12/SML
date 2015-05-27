# SVM for SML task...
library("kernlab")
library("graphics")
library("gplots")

source("pca_test.R")
source("normalize.R")
source("load_people_data.R")


svm_predict <- function(trainSet, trainVali, testSet, testVali, kern = "rbfdot", kernel_par = "automatic"){
	if((dim(testSet)[1] != length(testVali)) || (dim(trainSet)[1] != length(trainVali))){
		# generate error..
		e <- simpleError(paste(c("Bad input. Data and validation sets do not match in length."),collapse=""))
		stop(e)
	}	
	
	filter <- ksvm(trainSet, trainVali, type = "C-svc", cross = 0, kernel = kern, kpar = kernel_par) 
	
	preTest <- predict(filter, testSet)
	
	confus = array(0,c(10,10))
	
	per = 0
	for(i in 1:length(preTest)){
		confus[[preTest[i],testVali[i]]] = confus[[preTest[i],testVali[i]]] + 1;
		if(preTest[i] == (testVali)[i]){
			per = per + 1
		} 
	}
	per = per/length(testVali)
	
	return(list(succes = per, confusionTable = confus))
}


load = list(trainPartSize=200,testSize=200)
pca = list(breakpoint = 1)
norm = list(normMethod = "z-score")

data = prepareOneAloneNormPCA(3, 2, load, norm, pca , make_new = 1)


degree_v = 1:5
scale_v = seq(1, 100,10)/1000

#result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "polydot", kernel_par = list(degree = 1, scale=0.0001))

filename = "SVM_polydot_contour.RData"

if(T){
	startTimer <- proc.time()
	
	
	contour_data = matrix(0,length(degree_v),length(scale_v))
	
	for(deg in 1:length(degree_v)) {
		for(sc in 1:length(scale_v)) {
			result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "polydot", kernel_par = list(degree = degree_v[deg], scale=scale_v[sc])) # sigma
			contour_data[deg,sc] = result$succes
			
			# make vector
			point = which.max(contour_data)
			x_p = point %% length(degree_v)
			y_p = ceiling(point/length(degree_v))
			
			setEPS()
			
			postscript("contour_svm_poly.eps",height = 6, width = 8)
			# plot the contour plot
			filled.contour(y = scale_v, 
						   x = degree_v, 
						   contour_data, 
						   col=colorpanel(20, "black", "white"), 
						   levels=seq(min(contour_data), max(contour_data), length.out= 21),
						   locator={points(x = degree_v[x_p],y = scale_v[y_p], col = "red")}) 
			
			title(main = NULL, xlab = "Degree", ylab = "Scale")
			
			q = dev.off()
			
			tillnow = ((deg-1)*length(scale_v) + sc)
			total = length(degree_v)*length(scale_v)
			print(paste(c( tillnow, " / ", total , ". Time taken: ", ((proc.time() - startTimer)[3]), " / ", ((proc.time() - startTimer)[3])*(total)/(tillnow), " sec estimated time." ), collapse = "") )
		}
	}
	
	
	save(contour_data, file = filename)
}else {
	load(filename)
}

# make vector
point = which.max(contour_data)
x_p = point %% length(degree_v)
y_p = ceiling(point/length(degree_v))

setEPS()

postscript("contour_svm_poly.eps",height = 6, width = 8)
# plot the contour plot
filled.contour(y = scale_v, 
			   x = degree_v, 
			   contour_data, 
			   col=colorpanel(20, "black", "white"), 
			   levels=seq(min(contour_data), max(contour_data), length.out= 21),
			   locator={points(x = degree_v[x_p],y = scale_v[y_p], col = "red")}) 

title(main = NULL, xlab = "Degree", ylab = "Scale")

q = dev.off()


