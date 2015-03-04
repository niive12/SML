# 39m35.806s runtime
# (2.1.4) # Nikolaj
# time vs PC (optional: time & success vs PC)
# PCA: 10:10:380 dimensions
optional = TRUE 

source("load_people_data.R")
source("pca_test.R")

PC = seq(10, 380, 10)

fileName <- "time_vs_PC.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	time = array(0,length(PC))
	success = array(0,length(PC))
# 	people_data = prepareOne(group=3,member=1,trainPart=360,testPart=40) #1m53.803s runtime
	people_data = prepareOneAlone(3,1) #9 people vs 1 person             #39m35.806s runtime
# 	people_data = prepareAllMixed(360,40) #90:10
	for(i in 1:length(PC) ){
		time_tmp = proc.time()
		data = pca_simplification(people_data,noPC=PC[i])
		success[i] = run_knn(data,10)$success
		time_tmp = proc.time()-time_tmp
		time[i] = time_tmp[["elapsed"]]
	
	}
	save(success, time, file = fileName)
}

# 
colors= c("black","blue")
setEPS()
postscript("../../../Report/graphics/pca_timing.eps",height = 4, width = 8)
if ( optional == TRUE ) {
	# add margin so we can see both labels
	par(mar=c(5, 4, 4, 6) + 0.1)

	# plot time
	plot(PC, time, pch=16, ylim=c(min(time),max(time)), col=colors[1],
	axes=FALSE, type="o", xlab="", ylab="")
	axis(2, ylim=c(0,1),col="black",las=1)  # las=1 makes horizontal labels
	mtext("Time [s]",side=2,line=2.5)
	box()

	par(new=TRUE)

	# plot success
	plot(PC, success, pch=15, ylim=c(min(success),max(success)), col=colors[2],
	axes=FALSE, type="o", xlab="", ylab="")
	mtext("Success",side=4,col=colors[2],line=4) 
	axis(4, ylim=c(0,7000), col=colors[2],col.axis=colors[2],las=1)

	# Draw the axis
	axis(1,pretty(range(PC),10))
	mtext("Principal components",side=1,col="black",line=2.5)  

	legend("bottomright",legend=c("Time","Success"),pch=16:15,cex=0.8,col=colors)
} else {
	plot(PC,time,type="b",xlab="PC",ylab="time [s]",ylim=(c(0,max(time))),col=colors[1])
}
quiet = dev.off()