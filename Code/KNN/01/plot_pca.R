source("load_people_data.R")
source("pca_test.R")
data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
data = pca_simplification(data,noPC=50)

size = dim(data$trainSet)[1]/50
x_axis = array(0,size)
y_axis = array(0,size)

for( i in 1:size ){
	x_axis[i] = data$trainSet[i,][1]
}

xlabel = seq(round(min(x_axis)-0.05,digits=1),max(x_axis)+0.1,0.1) 
setEPS()
postscript("Rplots.pdf",height = 2, width = 8)
plot(x_axis, y_axis, pch=4, col="black", xlim=c(min(xlabel),max(xlabel)), axes=FALSE, xlab="", ylab="")
axis(1, at=xlabel)
q = dev.off()