# visuallize knn
library("graphics")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

pc_plot = 2
classes = c(6,2) # 6,9 and 10 interresting for 2
datasetsize = 5
N = 10


data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)

# normalize data
print("normalize")
data <- normalizeData(data, "z-score")
# pca
print("pca")
data <- pca_simplification(data, noPC = pc_plot)


coloooor = rainbow(length(classes))
limitx = c( min( c(data$trainSet[,1],data$testSet[,1]) ) , max( c(data$trainSet[,1], data$testSet[,1])) )
limity = c( min( c(data$trainSet[,2],data$testSet[,2]) ) , max( c(data$trainSet[,2], data$testSet[,2])) )

l = 1
while(data$testVali[l] != classes[2]){
	l = l + 1
}
cord = c(data$testSet[l,1], data$testSet[l,2])

setEPS()
postscript("knn_vis.eps",height = 6, width = 8)

plot(cord[1], cord[2], xlab = "PC 1", ylab = "PC 2", col = "black", xlim = limitx, ylim = limity)

closest = matrix(Inf, N, 1)

for(i in 1:length(data$trainSet[,1])){
	# test if it is that group
	classification = data$trainVali[i]
	for(j in 1:length(classes)){
		if(classes[j] == classification){
			points(data$trainSet[i,1], data$trainSet[i,2], col = coloooor[j])		
			d = dist(rbind(c(data$trainSet[i,1], data$trainSet[i,2]), cord))
			f = 1
			while(closest[f] > d && f <= N){
				if(f == 1){
					closest[f] = d	
				}
				else{
					closest[f - 1] = closest[f]
					closest[f] = d
				}
				f = f+1
			}
		}
	}
}

symbols(x = cord[1], y = cord[2], circles = closest[1], add = T, inches = F)
dev.off()

# plot
# colors = rainbow(pc_plot)
# setEPS()
# postscript("entropy_pc.eps",height = 6, width = 8)
# plot(1:divisions,entropy_data[1,], xaxt="n", xlab="Division", ylab="Entropy",type="l",col=colors[1],lty=1, ylim=c(min(entropy_data),max(entropy_data)), xlim=c(0,divisions+1))
# if(pc_plot > 1){
# 	for( i in 2:pc_plot) {
# 		lines(entropy_data[i,],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
# 	}
# 	legend("bottomright",NULL,name,cex=0.8,col=colors,lty=1:pc_plot)
# }
# axis(1, at=c(0,seq(10,divisions-10,10),divisions+1), labels=c("min", round((seq(10,divisions-10,10))/(divisions+1),2),"max"))
# q = dev.off()
