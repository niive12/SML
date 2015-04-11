# plot the entropy values for the first pca components given a number of divisions
library("graphics")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("entropy.R")


divisions = 200
pc_plot = 5

datasetsize = 400


data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)
# normalize data
data <- normalizeData(data, "z-score")
# pca
data <- pca_simplification(data, noPC = pc_plot)

#calc entropy
entropy_data <- matrix(0,pc_plot,divisions)
startTimer <- proc.time()
for( i in 1:pc_plot) {
	entropy_data[i,] = (entropy(data$trainSet[,i], data$trainVali, divisions)$entropy)
	print(paste(c(i , " / ", pc_plot, ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (pc_plot-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
}


# line name
name = 1:pc_plot
for(i in 1:pc_plot){
	name[i] = paste(c("PC",i),collapse="")
}

# plot
colors = rainbow(pc_plot)
setEPS()
postscript("entropy_pc.eps",height = 6, width = 8)
plot(1:divisions,entropy_data[1,], xlab="Division", ylab="Entropy",type="l",col=colors[1],lty=1, ylim=c(min(entropy_data),max(entropy_data)))
if(pc_plot > 1){
	for( i in 2:pc_plot) {
		lines(entropy_data[i,],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
	}
	legend("bottomright",NULL,name,cex=0.8,col=colors,lty=1:pc_plot)
}
q = dev.off()

plot(1:divisions,entropy_data[1,], xlab="Division", ylab="Entropy",type="l",col=colors[1],lty=1, ylim=c(min(entropy_data),max(entropy_data)))
if(pc_plot > 1){
	for( i in 2:pc_plot) {
		lines(entropy_data[i,],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
	}
	legend("bottomright",NULL,name,cex=0.8,col=colors,lty=1:pc_plot)
}