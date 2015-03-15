# 11h14m55.804s
# library("gplots") # needs to be installe
# elbowpoint: raw, zscore, smoothing and z-score & smoothing
library("graphics")
library("stats")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

# k = c(50,100,200,400)
# k = seq(50,800,50)
k = seq(100,4400,400)

kmeans_iterations = 500

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
if(file.exists(fileName) && 1){
	load(fileName)
} else{
# 	load(fileName)
	raw_homogenity = 1:length(k)
	z_homogenity = 1:length(k)
	s_homogenity = 1:length(k)
	zs_homogenity = 1:length(k)
	data <- prepareOneAlone(3,2, trainPartSize = 400, testSize = 400, peopleToLoad = people) #raw
	for(n in 1:length(k)){
		raw_homogenity[n] = 0;
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
			raw_homogenity[n] = raw_homogenity[n] + (digit[class]/sum(digit))/k[n];
		}
		print(raw_homogenity[n])
		save(raw_homogenity,z_homogenity,s_homogenity,zs_homogenity,file=fileName)
	}
	data = normalizeData(data, "z-score")
	for(n in 1:length(k)){
		z_homogenity[n] = 0;
		result = kmeans(data$trainSet, k[n], iter.max = kmeans_iterations)
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
			z_homogenity[n] = z_homogenity[n] + (digit[class]/sum(digit))/k[n];
		}
		print(z_homogenity[n])
		save(raw_homogenity,z_homogenity,s_homogenity,zs_homogenity,file=fileName)
	}
	smooth_data <- prepareOneAlone(3,2, trainPartSize = 400, testSize = 400, peopleToLoad = people, filter="gaussian", size=9,sigma=0.7) #smoothing
	for(n in 1:length(k)){
		s_homogenity[n] = 0;
		result = kmeans(smooth_data$trainSet, k[n])
		for(cluster_i in 1:k[n]) {
			digit = array(0,10);
			for(entry_i in 1:length(result$cluster)){
				if (result$cluster[entry_i] == cluster_i ){
					digit[smooth_data$trainVali[entry_i]] = digit[smooth_data$trainVali[entry_i]] + 1;
				}
			}
			class = 1
			for(d in 2:10){
				if(digit[d] > digit[class]){
					class = d;
				}
			}
			s_homogenity[n] = s_homogenity[n] + (digit[class]/sum(digit))/k[n];
		}
		print(s_homogenity[n])
		save(raw_homogenity,z_homogenity,s_homogenity,zs_homogenity,file=fileName)
	}
	smooth_data = normalizeData(smooth_data, "z-score")
	for(n in 1:length(k)){
		zs_homogenity[n] = 0;
		result = kmeans(smooth_data$trainSet, k[n])
		for(cluster_i in 1:k[n]) {
			digit = array(0,10);
			for(entry_i in 1:length(result$cluster)){
				if (result$cluster[entry_i] == cluster_i ){
					digit[smooth_data$trainVali[entry_i]] = digit[smooth_data$trainVali[entry_i]] + 1;
				}
			}
			class = 1
			for(d in 2:10){
				if(digit[d] > digit[class]){
					class = d;
				}
			}
			zs_homogenity[n] = zs_homogenity[n] + (digit[class]/sum(digit))/k[n];
		}
		print(zs_homogenity[n])
# 		print(paste(c(n, "/", length(k)," homogeneity: ", homogenity[n]),collapse =""))
		save(raw_homogenity,z_homogenity,s_homogenity,zs_homogenity,file=fileName)
	}
}
# 
test_name = c("raw", "z-score", "smoothed", "z-score+smoothed")

# # plot
colors = rainbow(4)
setEPS()
postscript("../../../Report/graphics/homogenity.eps",height = 4, width = 8)
plot(k, raw_homogenity,type="b",lty=1, col=colors[1],xlab="K clusters",ylab="Homogeneity [%]") 
lines(k,z_homogenity,  type="b",lty=2, col=colors[2])
lines(k,s_homogenity,  type="b",lty=3, col=colors[3])
lines(k,zs_homogenity, type="b",lty=4, col=colors[4])
legend("bottomright",test_name,cex=0.8,col=colors,lty=1:4)
q = dev.off()
setEPS()
postscript("../../../Report/graphics/heterogenity.eps",height = 4, width = 8)
plot(k, 1-raw_homogenity,   type="b",lty=1, col=colors[1],xlab="K clusters",ylab="heterogeneity [%]") 
lines(k,1-z_homogenity, type="b",lty=2, col=colors[2])
lines(k,1-s_homogenity, type="b",lty=3, col=colors[3])
lines(k,1-zs_homogenity,type="b",lty=4, col=colors[4])
legend("topright",test_name,cex=0.8,col=colors,lty=1:4)