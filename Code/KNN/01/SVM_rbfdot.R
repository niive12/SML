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

data = prepareOneAloneNormPCA(3, 2, load, norm, pca , make_new = 0)

sigma_v = seq(0, 6,0.25)

#result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "polydot", kernel_par = list(degree = 1, scale=0.0001))

filename = "SVM_rbfdot_line.RData"

if(T){
	startTimer <- proc.time()
	
	
	line_data = 1:length(sigma_v)
	for(i in 1:length(sigma_v)){
		line_data[i] = 0
	}
	
	
	for(sigma in 1:length(sigma_v)) {
		result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "rbfdot", kernel_par = list(sigma = sigma_v[sigma]))
		line_data[sigma] = result$succes
	
		# draw
		setEPS()
		
		postscript("lineplot_svm_poly.eps",height = 6, width = 8)
		# plot the contour plot
		plot(sigma_v,line_data, xlab="Sigma", ylab="Succes",type="l",lty=1)
		#axis(1, at=1:pc_plot, labels=x_lab)
		
		q = dev.off()
		
		print(paste(c( sigma, " / ", length(sigma_v) , ". Time taken: ", ((proc.time() - startTimer)[3]), " / ", ((proc.time() - startTimer)[3])*(length(sigma_v))/(sigma), " sec estimated time." ), collapse = "") )
	}
	
	
	save(line_data, file = filename)
}else {
	load(filename)
}


setEPS()

postscript("lineplot_svm_poly.eps",height = 6, width = 8)
# plot the contour plot
plot(sigma_v,line_data, xlab="Sigma", ylab="Succes",type="l",lty=1)
#axis(1, at=1:pc_plot, labels=x_lab)

q = dev.off()

