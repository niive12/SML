source("load_people_data.R")

data = prepareAllMixed(360,40)

N = dim(data$trainSet)[2]

data.pca = prcomp(data$trainSet, center=TRUE, scale=F)

sdev_sum_sum = cumsum(data.pca$sdev^2 / sum(data.pca$sdev^2))
breakpoint = 1;
NPC = length(sdev_sum_sum);
for(i in 1:length(sdev_sum_sum) ){
	if ( sdev_sum_sum[i] >= breakpoint ) {
		NPC = i;
		break
	}
}

 
# train_data = matrix(, c(N,NPC));

train_data = data.pca$x[,1:NPC]

# test_data = {data$testSet %*% data.pca$rotation}[,1:NPC]
test_data = {data$trainSet %*% data.pca$rotation}[,1:NPC]
 

res = knn(train_data, test_data, data$trainVali, 10)


# count right detections
per <- 0
for(i in 1:length(data$testVali)){
	if(res[i] == (data$testVali)[i]){
		per <- per + 1
# 		print(data$testVali[i])
	} 
}

print(per) #40
print(length(data$testVali)) #400
print(per/length(data$testVali)) #0.1
