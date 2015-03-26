library("KernSmooth") 
source("load_people_data.R")
source("normalize.R")

sum_of_digit <- function(digit) {
	sum = 0
	for( pixel in 1:length(digit) ) {
		sum = sum + digit[pixel];
	}
	return(sum)
}

conv_2d <- function(digit) {
	width = floor(sqrt(length(digit)));
	height = length(digit)/width;
	print(c("I am a fool",width,height))
	
	img = matrix(,height,width)
	
	for( d in 1:height) {
		img[d,] = digit[(d*width):((d-1)*width+1)]
	}
}


data = prepareOneAlone(3,2,trainPartSize=100, testSize=100, DPI=100, peopleToLoad = list(c(3,1),c(3,2)))
# data = normalizeData(data, "z-score")
new_trainset = matrix(0,(dim(data$trainSet)[1]/10),10)
for( i in 1:dim(data$trainSet)[1]) {
	new_trainset[[i %% dim(new_trainset)[1]+1,data$trainVali[i]]] = sum_of_digit(data$trainSet[i,])
}
# new_testset = array(0,length(data$testSet),10)
# for( i in 1:length(data$testSet)) {
# 	new_testset[i] = sum_of_digit(data$testSet[i,])
# }

estimated_pdf = list(c(1:10,1:10))
	
for( i in 1:10) {
	estimated_pdf[[i]] = bkde(x=new_trainset[,i], kernel="normal")
}

xmas = 1:10
xmis = 1:10
ymas = 1:10
ymis = 1:10
for(i in 1:10){
	xmas[i] = max(estimated_pdf[[i]]$x)
	xmis[i] = min(estimated_pdf[[i]]$x)
	ymas[i] = max(estimated_pdf[[i]]$y)
	ymis[i] = min(estimated_pdf[[i]]$y)
}

xmax = max(xmas)
xmin = min(xmis)
ymax = max(ymas)
ymin = min(ymis)
colors = rainbow(10)

plot(estimated_pdf[[1]], xlab="x", ylab="pdf", xlim=c(xmin,xmax), ylim=c(ymin,ymax),type="l",col=colors[1],lty=1)
for( i in 2:10) {
	lines(estimated_pdf[[i]],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
}
legend("topright",NULL,0:9,cex=0.8,col=colors,lty=1:3)