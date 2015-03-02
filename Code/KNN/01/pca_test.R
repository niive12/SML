source("load_people_data.R")

data = prepareAllMixed(360,40)

N = dim(data$trainSet)[2]

data.pca = prcomp(data$trainSet, center=TRUE, scale=FALSE)

sdev_sum_sum <- cumsum(data.pca$sdev^2 / sum((data.pca$sdev)^2))

print(sdev_sum_sum[1])

breakpoint <- 0.3
NPC <- length(sdev_sum_sum)
for(i in 1:length(sdev_sum_sum) ){
	if ( sdev_sum_sum[i] >= breakpoint ) {
		NPC <- i
		break
	}
}
if(NPC == 1){
	NPC = 2
}
print(paste(c("Number of dimensions for one letter: ",NPC),collapse=""))


# train_data = matrix(, c(N,NPC));

train_data = data.pca$x[,1:NPC]

# print(c("Centered..", data.pca$center))


test_data = ((data$testSet - data.pca$center) %*% data.pca$rotation)[,1:NPC]


print(c(dim(train_data),dim(test_data)))
res = knn(train_data, test_data, data$trainVali, k = 10)


# count right detections
per <- 0
for(i in 1:length(res)){
	if(res[i] == (data$testVali)[i]){
		per <- per + 1
		# 		print(data$testVali[i])
	} 
}

print(per) 
print(length(data$testVali)) 
print(per/length(data$testVali)) 

# print(data.pca$center)

