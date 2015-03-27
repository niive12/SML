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

probability <- function(data,pdf){
	small = 0
	if( data < min(pdf$x) ) {
		res = pdf$y[min(pdf$x)]
	} else if (data >= max(pdf$x) ) {
		res = pdf$y[max(pdf$x)]
	} else {
		for( s in 1:length(pdf$x) ){
			if ( data > pdf$x[s] ) {
				small = s
			} else {
				break
			}
		}
# 		res = (pdf$y[small]+pdf$y[small+1])/2 * (pdf$x[small+1]-pdf$x[small])
		res = pdf$y[small]
	}
	return(res)
}

sum_weighted <- function(digit) {
	sigma = 4;
	width = floor(sqrt(length(digit)));
	height = length(digit)/width;
	
	img = matrix(,height,width)
	
	for( d in 1:height) {
		img[d,] = digit[(d*width):((d-1)*width+1)]
	}
	
# 	com = centerOfMass(data$trainSet[i,],width)
	com = centerOfMass(digit,width)
	
	com$x = round(com$x)
	com$y = round(com$y)
# 	print(-com$x)
	kernel = matrix(0,width,height)
	centerx = ceiling(width/2)
	centery = ceiling(height/2)
	centery = centery
	
	a = 1
	b = 2
	for(x in (com$x-width+1):centerx ){
		for(y in (com$y-height):centery ){
# 			kernel[x+centerx,y+centery] = 1
# 			kernel[x+centerx,y+centery] = sqrt(x^2+y^2)*exp(-1*(x^2+y^2))
			kernel[x+centerx,y+centery] = (1/(2*pi*sigma^2))*exp(-1*((a*x)^2+(b*y)^2)/(2*sigma^2)) + (1/(2*pi*sigma^2))*exp(-1*((b*x)^2+(a*y)^2)/(2*sigma^2))
		}
	}
	
	img = kernel %*% img
	
	sum = sum_of_digit(img)
	return(sum)
}



kde_prepare <- function(group,member, DPI, peopleToLoad=getPeople(), noChars = 400 ){
	data = prepareOneAlone(3,2,trainPartSize=noChars, testSize=noChars, DPI=DPI, peopleToLoad = peopleToLoad )
	data = normalizeData(data, "bin",2)
	new_trainset = matrix(0,noChars,10)
	for( i in 0:(dim(data$trainSet)[1]-1)) {
		new_trainset[[i %% noChars+1+(floor(i/(10*noChars))*noChars),data$trainVali[i+1]]] = sum_weighted(data$trainSet[i+1,])
	}
	new_testset = matrix(0,(dim(data$testSet)[1]/10),10)
	for( i in 0:(dim(data$testSet)[1]-1)) {
		new_testset[[i %% noChars+1+floor(i/(10*noChars))*noChars,data$testVali[i+1]]] = sum_weighted(data$testSet[i+1,])
	}
	return(list(trainSet=new_trainset,testSet=new_testset, trainVali=data$trainVali, testVali=data$testVali))
}


kde_estimate <- function(data){
	confus = matrix(0,10,10)
	res = 1:10
	percent = 0
	estimated_pdf = list(c(1:10,1:10))
	for( i in 1:10) {
		estimated_pdf[[i]] = bkde(x=data$trainSet[,i], kernel="normal")
	}
	for( d in 1:length(data$testSet) ){
		for( i in 1:10) {
			res[i] = ( (probability(data$testSet[d], estimated_pdf[[i]])) )
		}
		
		detect = which.max(res) 
		
		if(detect == (data$testVali)[d]){
			percent = percent + 1
		} 
		confus[[detect,data$testVali[d]]] = confus[detect,data$testVali[d]] + 1;
	}
	percent = percent/length(data$testVali)
	
	trueDetections = array(0,10)
	noChars = 10
	for(i in 1:noChars){
		trueDetections[i] = (confus[i,i]/(length(data$testVali)/noChars))
	}

	return(list(confus=confus, percent=percent, trueDetections=trueDetections, estimated_pdf=estimated_pdf))
}

data = kde_prepare(3,2,DPI=100,peopleToLoad=list(c(3,1),c(3,2)),noChars=100)
res = kde_estimate(data)

xmas = 1:10;xmis = 1:10;ymas = 1:10;ymis = 1:10;
for(i in 1:10){
	xmas[i] = max(res$estimated_pdf[[i]]$x)
	xmis[i] = min(res$estimated_pdf[[i]]$x)
	ymas[i] = max(res$estimated_pdf[[i]]$y)
	ymis[i] = min(res$estimated_pdf[[i]]$y)
}
xmax = max(xmas);xmin = min(xmis);ymax = max(ymas);ymin = min(ymis)
colors = rainbow(10)

print(res$confus)
print(res$percent)
print(res$trueDetections)

# write.latex(res$confus, 0:9, 0:9, "../../../Report/graphics/kde_confus.tex")
# setEPS()
# postscript("../../../Report/graphics/kde_graphs_sum.eps",height = 4, width = 8)
plot(res$estimated_pdf[[1]], xlab="x", ylab="pdf", xlim=c(xmin,xmax), ylim=c(ymin,ymax),type="l",col=colors[1],lty=1)
for( i in 2:10) {
	lines(res$estimated_pdf[[i]],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
}
legend("topright",NULL,0:9,cex=0.8,col=colors,lty=1:10)
q = dev.off()