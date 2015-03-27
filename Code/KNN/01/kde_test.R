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
			kernel[x+centerx,y+centery] = 1
# 			kernel[x+centerx,y+centery] = sqrt(x^2+y^2)*exp(-1*(x^2+y^2))
# 			kernel[x+centerx,y+centery] = (1/(2*pi*sigma^2))*exp(-1*((a*x)^2+(b*y)^2)/(2*sigma^2)) + (1/(2*pi*sigma^2))*exp(-1*((b*x)^2+(a*y)^2)/(2*sigma^2))
		}
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
# 	new_trainset[[i %% dim(new_trainset)[1]+1,data$trainVali[i]]] = sum_of_digit(data$trainSet[i,])
	new_trainset[[i %% dim(new_trainset)[1]+1,data$trainVali[i]]] = sum_weighted(data$trainSet[i,])
}
new_testset = matrix(0,(dim(data$testSet)[1]/10),10)
for( i in 1:dim(data$testSet)[1]) {
# 	new_testset[[i %% dim(new_testset)[1]+1,data$testVali[i]]] = sum_of_digit(data$testSet[i,])
	new_testset[[i %% dim(new_testset)[1]+1,data$testVali[i]]] = sum_weighted(data$testSet[i,])
}

estimated_pdf = list(c(1:10,1:10))

train_x = matrix(0,dim(data$trainSet)[1])
for( i in 1:dim(data$trainSet)[1] ){
	train_x[i] = sum_of_digit(data$trainSet[i,])
# 	train_x[i] = sum_weighted(data$trainSet[i,])
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

confus = matrix(0,10,10)
res = 1:10
percent = 0
for( d in 1:length(new_testset) ){
	for( i in 1:10) {
		res[i] = ( (probability(new_testset[d], estimated_pdf[[i]])) )
# 		res[i] = ( (probability(new_testset[d], estimated_pdf[[i]])) / probability(new_testset[d], pdf_x) )
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
# print(confus)
print(percent)
print(trueDetections)
write.latex(confus, 0:9, 0:9, "../../../Report/graphics/kde_confus.tex")

setEPS()
postscript("../../../Report/graphics/kde_graphs_sum.eps",height = 4, width = 8)
plot(estimated_pdf[[1]], xlab="x", ylab="pdf", xlim=c(xmin,xmax), ylim=c(ymin,ymax),type="l",col=colors[1],lty=1)
for( i in 2:10) {
	lines(estimated_pdf[[i]],col=colors[i],lty=i)#, xlab="x", ylab="pdf")
}
legend("topright",NULL,0:9,cex=0.8,col=colors,lty=1:10)
q = dev.off()