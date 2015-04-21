# 4h48m35.934s
source("C50predict.R")
source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

fileName <- "time_vs_tree_entropy.RData"
# fileName <- "time_vs_tree.RData"

ntrees = 1:30

load_setting        = list(trainPartSize = 400, testSize = 400)
# normalize_setting = list(normMethod = "z-score")
normalize_setting = list(normMethod = "entropy")
pca_setting         = list(noPC=50)


if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	time = array(0,length(ntrees))
	success = array(0,length(ntrees))	
	data = prepareOneAloneNormPCA(3,2,load_setting,normalize_setting,pca_setting)
	
	for(i in 1:length(ntrees) ){
		time_tmp = proc.time()
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali), trials=ntrees[i])
		time_tmp = proc.time()-time_tmp
		time[i] = time_tmp[["user.self"]]
		print(c(ntrees[i], time[i]))
	
		success[i] = random_forrest_predict(data=data, model=model)$success
	}
	save(success, time, file = fileName)
}

colors= c("black","blue")
setEPS()
postscript("../../../Report/graphics/tree_timing_entropy.eps",height = 4, width = 8)
# postscript("../../../Report/graphics/tree_timing.eps",height = 4, width = 8)
	# add margin so we can see both labels
	par(mar=c(5, 4, 4, 6) + 0.1)

	# plot time
	plot(ntrees, time, pch=16, ylim=c(min(time),max(time)), col=colors[1],
	axes=FALSE, type="o", xlab="", ylab="")
	axis(2, ylim=c(0,1),col="black",las=1)  # las=1 makes horizontal labels
	mtext("Time [s]",side=2,line=2.5)
	box()

	par(new=TRUE)

	# plot success
	plot(ntrees, success, pch=15, ylim=c(min(success),max(success)), col=colors[2],
	axes=FALSE, type="o", xlab="", ylab="")
	mtext("Success",side=4,col=colors[2],line=4) 
	axis(4, ylim=c(0,7000), col=colors[2],col.axis=colors[2],las=1)

	# Draw the axis
	axis(1,pretty(range(ntrees),10))
	mtext("Trees",side=1,col="black",line=2.5)  

	legend("bottomright",legend=c("Time","Success"),pch=16:15,cex=0.8,col=colors)
quiet = dev.off()