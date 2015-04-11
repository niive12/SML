source("load_people_data.R")
source("normalize.R")
source("pca_test.R")
source("Baye.R")

PCA = 0.5
# bins = 2:256
start        = seq(10,100,5)
high_numbers = seq(150,250,50)
bins = c(2:9,start,high_numbers)
print(length(bins))
fileName <- "data_time_vs_bins_baye.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
# 	data = prepareAllMixed(360,40,peopleToLoad = getPeople())
	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, peopleToLoad = getPeople() )
	print("data retrieved")
	data = normalizeData(data, "z-score")
	print("z scored")
	data = pca_simplification(data,breakpoint=PCA)
	print("simplified successful")
	save(data, file = fileName)
}
# test_result <- "data_test_time_vs_bins_baye.RData"
test_result <- "new_data_test_time_vs_bins_baye.RData"
if ( file.exists(test_result) && 0 ) {
	print(paste(c("test data exists in ", test_result),collapse=""))
	load(test_result)
} else {
	time = array(0,length(bins))
	success = array(0,length(bins))
	for( i in 1:length(bins) ) {
		time_tmp = proc.time()
		bdata = normalizeData(data,"bin", bins[i])
		print(c(i,"data is in the bin"))
		time_tmp = proc.time()-time_tmp
		time[i] = time_tmp[["user.self"]]
		success[i] = baye_predict(bdata)$success
	}
	save(time,success, file=test_result)
}

x_axis = bins
y1_axis = success
y1_name = "Success"
y1_unit = "[%]"
y2_axis = time
y2_name = "Time"
y2_unit = "[s]"

plotstyle = "l"

colors= c("black","blue")
setEPS()
postscript("../../../Report/graphics/new_baye_timing_bins.eps",height = 4, width = 8)

	# add margin so we can see both labels
	par(mar=c(5, 4, 4, 6) + 0.1)

	# plot time
	plot(x_axis, y1_axis, pch=16, ylim=c(min(y1_axis),max(y1_axis)), col=colors[1],
	axes=FALSE, type=plotstyle, xlab="", ylab="")
	axis(2, ylim=c(0,1),col=colors[1],las=1)  # las=1 makes horizontal labels
	mtext(paste(c(y1_name," ",y1_unit),collapse=""),side=2,line=2.5)
	box()

	par(new=TRUE)

	# plot success
	plot(x_axis, y2_axis, pch=15, ylim=c(min(y2_axis),max(y2_axis)), col=colors[2],
	axes=FALSE, type=plotstyle, xlab="", ylab="")
	mtext(paste(c(y2_name," ",y2_unit),collapse=""),side=4,col=colors[2],line=4) 
	axis(4, ylim=c(0,7000), col=colors[2],col.axis=colors[2],las=1)

	# Draw the axis
	axis(1,pretty(range(bins),10))
	mtext("Bins",side=1,col="black",line=2.5)  

	legend("bottomright",legend=c(y1_name,y2_name),pch=16:15,cex=0.8,col=colors)

quiet = dev.off()