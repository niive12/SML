# test for the final report
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")


testSetSize = 40
trainSetSize = 360


# pca test,, comp sigma vs PC
if(F){
	s_k = 1
	s_size = 5
	s_sigma = 0.9
	data_pc = c(seq(2,80,2))
	pc_knn_success = matrix(0,2,length(data_pc))
	pc_knn_time = 1:length(data_pc)
	
	data_s = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, filter = "gaussian", sigma = s_sigma, size = s_size, make_new = 1)
	data_n = prepareOne(3, 2, trainPart = trainSetSize, testPart = testSetSize, make_new = 1)
	
	for(f_pc in 1:length(data_pc)){
		print(paste(c("Running PC contour: ", f_pc, " / ", length(data_pc)), collapse = ""))
		
		data_sim_pc = pca_simplification(data_s, noPC=data_pc[f_pc]);
		
		time_tmp = proc.time()
		
		knn_res = run_knn(data_sim_pc, s_k)
		
		time_tmp = proc.time()-time_tmp
		pc_knn_time[f_pc] = time_tmp[[3]]
		
		pc_knn_success[1,f_pc] = knn_res$success
		
		data_sim_pc = pca_simplification(data_n, noPC=data_pc[f_pc]);
		
		knn_res = run_knn(data_sim_pc, s_k)
		
		pc_knn_success[2,f_pc] = knn_res$success
		
		save(data_pc, pc_knn_success, file = "KNN_final_pca.RData")
	}
	
}else{
	load("KNN_final_pca.RData")
}

colors = c("black", rainbow(2))
setEPS()
postscript("../../../Report/graphics/knn_pc.eps",height = 4, width = 8)
par(mar=c(4, 5, 2, 5) + 0.1)
# plot time
plot(data_pc, pc_knn_time, pch=16, ylim=c(min(pc_knn_time),max(pc_knn_time)), col=colors[1],
	 axes=FALSE, type="o", xlab="", ylab="")
axis(2, ylim=c(0,1),col="black",las=1)  # las=1 makes horizontal labels
mtext("Time [s]",side=2,line=3.5)
box()

par(new=TRUE)

# plot success
plot(data_pc, pc_knn_success[1,], pch=15, ylim=c(min(pc_knn_success),max(pc_knn_success)), col=colors[2],
	 axes=FALSE, type="o", xlab="", ylab="")
par(new=TRUE)
plot(data_pc, pc_knn_success[2,], pch=15, ylim=c(min(pc_knn_success),max(pc_knn_success)), col=colors[3],
	 axes=FALSE, type="o", xlab="", ylab="")
mtext("Success",side=4,col="darkblue",line=3) 
axis(4, col="darkblue",col.axis="darkblue",las=1)

# Draw the axis
axis(1,pretty(range(data_pc),3))
mtext("Principal Components",side=1,col="black",line=2.5)  

legend("bottomright",legend=c("Time","Smoothed", "Raw"),pch=16:15,cex=0.8,col=colors)

quiet = dev.off()
