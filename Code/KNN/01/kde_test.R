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
	big   = 0
	if( data < min(pdf$x) ) {
		res = pdf$y[min(pdf$x)]
	} else if (data > max(pdf$x) ) {
		res = pdf$y[max(pdf$x)]
	} else {
		for( s in 1:length(pdf$x) ){
			if ( data > s ) {
				small = pdf$x[s]
			} else {
				break
			}
		}
		(pdf$y[s]+pdf$y[s+1])/2 * (pdf$x[s+1]-pdf$x[s])
	}
}

sum_weighted <- function(digit) {
	sigma = 4;
	width = floor(sqrt(length(digit)));
	height = length(digit)/width;
	
	img = matrix(,height,width)
	
	for( d in 1:height) {
		img[d,] = digit[(d*width):((d-1)*width+1)]
	}
	
	com = centerOfMass(data$trainSet[i,],width)
	
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
# 			kernel[x+centerx,y+centery] = 
# 			kernel[x+centerx,y+centery] = sqrt(x^2+y^2)*exp(-1*(x^2+y^2))
			kernel[x+centerx,y+centery] = (1/(2*pi*sigma^2))*exp(-1*((a*x)^2+(b*y)^2)/(2*sigma^2)) + (1/(2*pi*sigma^2))*exp(-1*((b*x)^2+(a*y)^2)/(2*sigma^2))
		}
	}
	
	for(i in 1:length(img)){
		img[i] = img[i] -1
	}
	img = kernel %*% img
	
	sum = sum_of_digit(img)
	return(sum)
}


data = prepareOneAlone(3,2,trainPartSize=100, testSize=100, DPI=100, peopleToLoad = list(c(3,1),c(3,2)))
data = normalizeData(data, "bin",2)
new_trainset = matrix(0,(dim(data$trainSet)[1]/10),10)
# for( i in 1:dim(data$trainSet)[1]) {
# 	new_trainset[[i %% dim(new_trainset)[1]+1,data$trainVali[i]]] = sum_of_digit(data$trainSet[i,])
# }

for( i in 1:dim(data$trainSet)[1]) {
	new_trainset[[i %% dim(new_trainset)[1]+1,data$trainVali[i]]] = sum_weighted(data$trainSet[i,])
}
new_testset = matrix(0,(dim(data$testSet)[1]/10),10)
for( i in 1:dim(data$testSet)[1]) {
	new_testset[[i %% dim(new_testset)[1]+1,data$testVali[i]]] = sum_weighted(data$testSet[i,])
}

estimated_pdf = list(c(1:10,1:10))

train_x = matrix(0,dim(data$trainSet)[1])
for( i in 1:dim(data$trainSet)[1] ){
	train_x[i] = sum_weighted(data$trainSet[i,])
}
pdf_x = bkde(x=train_x, kernel="normal")

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

for( i in 1:10) {
	print( (probability(new_testset[1], estimated_pdf[[i]]) * 0.1) / probability(new_testset[1], pdf_x) )
}
# print(probability(new_testset[1], estimated_pdf[[1]]) )
# print(probability(new_testset[1], pdf_x) )
# print(probability(new_testset[1], estimated_pdf[[2]]) )
# print(probability(new_testset[1], estimated_pdf[[3]]) )
# print(probability(new_testset[1], estimated_pdf[[4]]) )
# print(probability(new_testset[1], estimated_pdf[[5]]) )
# print(probability(new_testset[1], estimated_pdf[[6]]) )
# print(probability(new_testset[1], estimated_pdf[[7]]) )
# print(probability(new_testset[1], estimated_pdf[[8]]) )
# print(probability(new_testset[1], estimated_pdf[[9]]) )
# print(probability(new_testset[1], estimated_pdf[[10]]) )





plot(estimated_pdf[[1]], xlab="x", ylab="pdf", xlim=c(xmin,xmax), ylim=c(ymin,ymax),type="l",col=colors[1],lty=1)
for( i in 2:10) {
	lines(estimated_pdf[[i]],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
}
legend("topright",NULL,0:9,cex=0.8,col=colors,lty=1:3)