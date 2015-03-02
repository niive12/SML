source("load_people_data.R")

data = prepareAllMixed(360,40)

N = dim(data$trainSet)[2]

data.pca = prcomp(data$trainSet, center=T, scale=F)

sdev_sum_sum = cumsum(data.pca$sdev^2 / sum(data.pca$sdev^2))
breakpoint = 0.86
NPC = length(sdev_sum_sum);
for(i in 1:length(sdev_sum_sum) ){
	if ( sdev_sum_sum[i] >= breakpoint ) {
		NPC = i;
		break
	}
}

print(c("NPC: ",NPC))

data.pca = prcomp(data$trainSet, center=F, scale=F)

train_data = data.pca$x[,1:NPC]

test_data = data$testSet %*% data.pca$rotation[,1:NPC]


res = knn(train_data, test_data, data$trainVali, k=10)

# print(res);
# count right detections

confus = array(0,c(10,10))

per <- 0
for(i in 1:length(data$testVali)){
	confus[[data$testVali[i],res[i]]] = confus[data$testVali[i],res[i]] + 1;
	if(res[i] == (data$testVali)[i]){
		per <- per + 1
# 		print(data$testVali[i]-1)
	} 
}

print(confus) 

# print(per) #40
# print(length(data$testVali)) #400
print(per/length(data$testVali)) #0.1
