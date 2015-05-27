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

ppl = getPeople()
res = matrix(0,3,length(ppl))

k_rbf = list(sigma = 2)
k_poly = list(degree = 2, scale = 0.02)
k_vanilla = list()

x_lab <- 1:length(ppl)
for(i in 1:length(ppl)){
	x_lab[i] <- paste(c(ppl[[i]][1],":",ppl[[i]][2]),collapse="")
}

filename = "SVM_kernel_comp.RData"

if(T){
	load(filename)
	startTimer <- proc.time()
	
	
	for(person in 1:length(ppl)) {
		data = prepareOneAloneNormPCA(ppl[[person]][1], ppl[[person]][2], load, norm, pca , make_new = 0)
		
		if(T){
			result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "rbfdot", kernel_par = k_rbf)
			res[1,person] = result$succes
		}
		if(T){
			result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "polydot", kernel_par = k_poly)
			res[2,person] = result$succes
		}
		if(F){
			result = svm_predict(data$trainSet, data$trainVali, data$testSet, data$testVali, kern = "vanilladot", kernel_par = k_vanilla)
			res[3,person] = result$succes
		}
		# pre plot
		colors = rainbow(3)
		setEPS()
		postscript("svm_kernel_comp.eps",height = 6, width = 8)
		
		plot(1:length(ppl),res[1,], xaxt="n",type="b", xlab="Person", ylab="Success Rate [%]", col = colors[1], ylim=c(min(res),max(res)))
		lines(1:length(ppl),res[2,],type="b",lty=1, col=colors[2])
		lines(1:length(ppl),res[3,],type="b",lty=1, col=colors[3])
		axis(1, at=1:length(ppl), labels=x_lab)
		legend("topright",legend=c("rbfdot","polydot","vanilladot"),pch=16:14,cex=0.8,col=colors)
		
		q = dev.off()
		
		print(paste(c( person, " / ", length(ppl) , ". Time taken: ", ((proc.time() - startTimer)[3]), " / ", ((proc.time() - startTimer)[3])*(length(ppl))/(person), " sec estimated time." ), collapse = "") )
	}
	
	
	save(res, file = filename)
}else {
	load(filename)
}

print(c("mean rbf: ", mean(res[1,])))
print(c("mean poly: ", mean(res[2,])))
print(c("mean vanilla: ", mean(res[3,])))

colors = rainbow(3)
setEPS()
postscript("svm_kernel_comp.eps",height = 6, width = 8)

plot(1:length(ppl),res[1,], xaxt="n",type="b", xlab="Person", ylab="Success Rate [%]", col = colors[1], ylim=c(min(res),max(res)))
lines(1:length(ppl),res[2,],type="b",lty=1, col=colors[2])
lines(1:length(ppl),res[3,],type="b",lty=1, col=colors[3])
axis(1, at=1:length(ppl), labels=x_lab)
legend("topright",legend=c("rbfdot","polydot","vanilladot"),pch=16:14,cex=0.8,col=colors)

q = dev.off()