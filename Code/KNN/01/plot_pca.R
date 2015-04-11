source("load_people_data.R")
source("pca_test.R")
source("entropy.R")
data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
size = length(data$trainVali)/10
data = pca_simplification(data,noPC=50)


length = size;

x_axis = matrix(0,size,10)

digit = 3

for( i in 1:size ){
	for(d in 0:9){
		x_axis[i,d+1] = data$trainSet[i+d*length,][1]
	}
}
div = entropy(component=data$trainSet[,digit], classifications=data$trainVali, divisions = 200)

xlabel = seq(round(min(x_axis)-0.05,digits=1),max(x_axis)+0.1,0.1) 

colors = rainbow(10)
setEPS()
postscript("../../../Report/graphics/decision_seperation.eps",height = 4, width = 8)
for(i in 1:10){
	if(i == 1){
		plot(x_axis[,i], array(i,size), pch=4, col=colors[i], xlim=c(min(xlabel),max(xlabel)+0.6), ylim=c(0,10), axes=FALSE, xlab="", ylab="")
	} else {
		lines(x_axis[,i], array(i,size), type="p", pch=4, col=colors[i])
	}
}
segments(div$divider, 0 , div$divider, 20, col="black")
axis(1, at=xlabel)
legend("topright",NULL,9:0,cex=0.95,col=rev(colors),lty=array(1,10))
q = dev.off()