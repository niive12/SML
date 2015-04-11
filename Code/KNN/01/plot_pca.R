source("load_people_data.R")
source("pca_test.R")
source("entropy.R")
data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
size = length(data$trainVali)/10
data = pca_simplification(data,noPC=50)


length = size;

x_axis = matrix(0,size,10)



for( i in 1:size ){
	for(d in 0:9){
		x_axis[i,d+1] = data$trainSet[i+d*length,][1]
	}
}
print(dim(data$trainSet))
div = entropy(component=data$trainSet[,1], classifications=data$trainVali, divisions = 200)
print(div)

xlabel = seq(round(min(x_axis)-0.05,digits=1),max(x_axis)+0.1,0.1) 

colors = rainbow(10)
setEPS()
postscript("Rplots.pdf",height = 4, width = 8)
for(i in 1:10){
	if(i == 1){
		plot(x_axis[,i], array(i,size), pch=4, col="black", xlim=c(min(xlabel),max(xlabel)), ylim=c(0,10), axes=FALSE, xlab="", ylab="")
	} else {
		lines(x_axis[,i], array(i,size), type="p", pch=4, col=colors[i])
	}
}
segments(div$divider, 0 , div$divider, 20, col="red")
axis(1, at=xlabel)
q = dev.off()

pc1 = matrix(0,size,10)
for(i in 1:size ) {
	for( d in 0:9 ){
		pc1[i,d+1] = data$trainSet[i+d*length,][1]
	}
}
for(i in 1:10 ){
	print(c(i-1,mean(pc1[,i]),var(pc1[,i])))
}