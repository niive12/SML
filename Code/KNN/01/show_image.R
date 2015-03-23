library("gplots")
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
	print(sum)
	
	setEPS()
	postscript(filename,height = height, width = width)
	image(img, xaxt="n", yaxt="n",col=colorpanel(256, "black", "white"))
	q = dev.off()
}

source("load_people_data.R")
source("normalize.R")
data = prepareOneAlone(3,2,trainPartSize=400, testSize=400, DPI=100, make_new = 1, peopleToLoad = list(c(3,1),c(3,2)))
print(c("Length: ", length(data$testSet[1,])))
show_image(data$testSet[826,],"../../../Report/graphics/bins_inf.eps",width=20)
bdata = normalizeData(data,"bin", 10)
show_image(bdata$testSet[826,],"../../../Report/graphics/bins_10.eps", width=20)
bdata = normalizeData(data,"bin", 5)
show_image(bdata$testSet[826,],"../../../Report/graphics/bins_5.eps", width=20)
bdata = normalizeData(data,"bin", 2)
show_image(bdata$testSet[826,],"../../../Report/graphics/bins_2.eps", width=20)
