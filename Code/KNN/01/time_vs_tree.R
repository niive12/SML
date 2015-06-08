# 4h48m35.934s
source("C50predict.R")
source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

# fileName <- "time_vs_tree_entropy_2.RData"
fileName <- "boosting.RData"

trials = seq(2,100,1)

pca_setting         = list(noPC=50)


if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
	success = success_test
} else {
	time = array(0,length(trials))
	success = array(0,length(trials))
	success_train = array(0,length(trials))
	data = prepareOne(3,2,360,40,DPI=100,filter="gaussian", make_new=1,sigma=0.9, size=7)
	data = prepare_data_for_tree(data)
	
	for(i in 1:length(trials) ){
		time_tmp = proc.time()
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali),trials=trials[i]
		                 ,control=C5.0Control(minCases=7))
		time_tmp = proc.time()-time_tmp
		time[i] = time_tmp[["user.self"]]
	
		success[i] = tree_predict(data=data, model=model)$success
		success_train[i] = tree_predict_train(data=data,model=model)
		print(c(trials[i], time[i],success[i],success_train[i]))
	}
	save(success_train, success, time, file = fileName)
}

# trials =               trials[2:60]
# time =                   time[2:60]
# success =             success[2:60]
# success_train = success_train[2:60]
colors= c("black","blue")
setEPS()
postscript("../../../Report/graphics/tree_timing_one.eps",height = 4, width = 8)
# postscript("../../../Report/graphics/tree_timing.eps",height = 4, width = 8)
	# add margin so we can see both labels
	par(mar=c(5, 4, 4, 6) + 0.1)

	# plot time
	plot(trials, success, pch=16, ylim=c(min(success),max(success)), col=colors[1],
	axes=FALSE, type="o", xlab="", ylab="")
	axis(2, ylim=c(0,1),col="black",las=1)  # las=1 makes horizontal labels
	mtext("Success",side=2,line=3)
	box()

	par(new=TRUE)

	# plot success
	plot(trials, time, pch=15, ylim=c(min(time),max(time)), col=colors[2],
	axes=FALSE, type="o", xlab="", ylab="")
	mtext("Time [s]",side=4,col=colors[2],line=3) 
	axis(4, ylim=c(0,7000), col=colors[2],col.axis=colors[2],las=1)

	# Draw the axis
	axis(1,pretty(range(trials),10))
	mtext("Trials",side=1,col="black",line=2.5)  

	legend("bottomright",legend=c("Success","Time"),pch=16:15,cex=0.8,col=colors)
quiet = dev.off()

setEPS()
postscript("../../../Report/graphics/tree_boost_overfitting.eps",height = 4, width = 8)
	# add margin so we can see both labels
	par(mar=c(5, 4, 4, 6) + 0.1)

	# plot time
	plot(trials, success, pch=16, ylim=c(min(c(min(success),min(success_train))),max(c(max(success),max(success_train)))), col=colors[1],
	type="l", xlab="Trials", ylab="Success")
	lines(trials, success_train, pch=15, col=colors[2], type="l")
	legend("bottomright",legend=c("Test","Training"),pch=16:15,cex=0.8,col=colors)
quiet = dev.off()