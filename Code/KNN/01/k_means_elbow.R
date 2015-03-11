# library("gplots") # needs to be installe
library("graphics")
library("stats")

source("load_people_data.R")
source("pca_test.R")

# k = c(50,100,200,400)
k = seq(50,800,50)

people <- getPeople()
noPeople <- length(people)

result <- matrix(0,4,noPeople-1)

# make unique filename
fileName <- "homogenity-test"

for(i in 1:noPeople){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")



# run test
if(file.exists(fileName) && 0){
	load(fileName)
} else{
	data <- prepareOneAlone(3,2, trainPartSize = 400, testSize = 400, peopleToLoad = people)
	homogenity = 1:length(k)
	for(n in 1:length(k)){
		homogenity[n] = 0;
		result = kmeans(data$trainSet, k[n])
		for(cluster_i in 1:k[n]) {
			digit = array(0,10);
			for(entry_i in 1:length(result$cluster)){
				if (result$cluster[entry_i] == cluster_i ){
					digit[data$trainVali[entry_i]] = digit[data$trainVali[entry_i]] + 1;
				}
			}
			class = 1
			for(d in 2:10){
				if(digit[d] > digit[class]){
					class = d;
				}
			}
			homogenity[n] = homogenity[n] + (digit[class]/sum(digit))/k[n];
		}
		print(homogenity[n])
		save(homogenity,file=fileName)
	}
}
# 
# # plot
postscript("homogenity.eps",height = 6, width = 8)
plot(k, homogenity,type="b",xlab="K clusters",ylab="Homogenity [%]") 
q = dev.off()
postscript("heterogenity.eps",height = 6, width = 8)
plot(k, 1-homogenity,type="b",xlab="K clusters",ylab="heterogenity [%]") 
q = dev.off()
