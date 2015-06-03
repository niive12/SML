library("gplots")
source("load_people_data.R")

show_image <- function(digit, filename="image.eps", width = 18) {
	height = length(digit)/width;

# 	print(height)
# 	print(width)
	img = matrix(,height,width)
	
	for( d in 1:height) {
		img[d,] = digit[(d*width):((d-1)*width+1)]
	}
	
	sum = 0
	for( y in 1:height ) {
		for ( x in 1:width ){
			sum = sum + img[x,y]
		}
	}
	print(c("sum: ",sum))
	
	setEPS()
	postscript(filename,height = height, width = width)
	image(img, xaxt="n", yaxt="n",col=colorpanel(256, "black", "white"))
	q = dev.off()
}

source("load_people_data.R")
source("normalize.R")

sig = array(1,4)
size = c(3,5,7,9)

# data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, filter="gaussian",sigma=2,size=5, make_new = 1, peopleToLoad = list(c(3,1),c(3,2)))
# show_image(data$testSet[826,],filename="../../../Report/graphics/smooth_2_5.eps",width=20)
# 
# 
# data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, make_new = 1, peopleToLoad = list(c(3,1),c(3,2)))
# show_image(data$testSet[826,],"../../../Report/graphics/smooth_none.eps",width=20)
# for( i in 1:length(sig) ){
# 	filename = paste(c("../../../Report/graphics/smooth_",sig[i],"_",size[i]),collapse="")
# 	print(filename)
# 	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, filter="gaussian",sigma=sig[i],size=size[i], make_new = 1, peopleToLoad = list(c(3,1),c(3,2)))
# 	show_image(data$testSet[826,],filename=filename,width=20)
# }
# 
# sig = seq(0.5,0.9,0.1)
# size = c(5,5,5,5)
# 
# for( i in 1:length(sig) ){
# 	filename = paste(c("../../../Report/graphics/smooth_",sig[i],"_",size[i]),collapse="")
# 	print(filename)
# 	data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, filter="gaussian",sigma=sig[i],size=size[i], make_new = 1, peopleToLoad = list(c(3,1),c(3,2)))
# 	show_image(data$testSet[826,],filename=filename,width=20)
# }