#  takes about 6h in full resolution (400 400) for 12 ppl
#  takes about 10h30 in full resolution (400 400) for 16 ppl

library("graphics")
library("parallel") 

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")


# compare one from dataset to all others (do with 10 different ppl)
# plot sucess for that person
# do with normalization before and after with both normalizations
# best plot: k = 19, PCA = 0.8
# prepareOneAlone(g,m, trainPartSize = 100, testSize = 40, peopleToLoad = getPeople())

# take plot way from: results.R
# b = streg imellem, o = streg igennem


PCA = 0.8
k = 10



# prep for data
people <- getPeople()
noPeople <-length(people)
result <- matrix(0,5,noPeople)

fileName <- "norm-test_"

for(i in 1:length(people)){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")


if(file.exists(fileName) && 0){
	load(fileName)
} else{
	startTime <- proc.time() # used for timing
	# make loop to save data
	for(person in 1:noPeople){
		data <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = 400, testSize = 400, peopleToLoad = people,make_new=1)
		
		# min max
		# 	print("min max")
		preNormalized <- normalizeData(data, "min-max")
		preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
		
		postNormalized <- pca_simplification(data, breakpoint = PCA)
		postNormalized <- normalizeData(postNormalized, "min-max")
		
		result[1,person] <- run_knn(preNormalized,k)$success # minmax pre
		result[2,person] <- run_knn(postNormalized,k)$success # minmax post
		
		# z-score
		# 	print("z score")
		preNormalized <- normalizeData(data, "z-score")
		preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
		
		postNormalized <- pca_simplification(data, breakpoint = PCA)
		postNormalized <- normalizeData(postNormalized, "z-score")
		
		result[3,person] <- run_knn(preNormalized,k)$success # zscore pre
		result[4,person] <- run_knn(postNormalized,k)$success # zscore post
		
		# 	print("no normalization")
		result[5,person] <- run_knn(data,k)$success # none
		
		timer <- (((proc.time() - startTime)[3])*(noPeople-person)/person)
		print(paste(c(person, "/", noPeople, " datasets loaded. Estimated finish time: ",timer, " seconds."),collapse = ""))
		
		# save the data
		save(result, file = fileName)
	}
}


print(result)

# mean
mean <- 1:5

for(i in 1:5){
	mean[i] <- mean(result[i,])
}

print("mean")
print(mean)
# 0.44050 0.33925 0.47000 0.40975 0.38300 # 100 40 test K = 19
# 0.5032083 0.4072083 0.5439375 0.4913542 0.4149167 # 400 400 test K = 19
# 0.5532031 0.4493750 0.5874687 0.5339844 0.4791094 # 400 400 test 16 ppl K = 19
#  K = 10

#result

#make x string
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

print(x_lab)

colors = rainbow(5)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
legend("bottomright",c("min-max pre","min-max post","z-score pre","z-score post","none"),cex=0.8,col=colors,lty=1:4)
for(i in 2:5){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
setEPS()
postscript("graph_normalization.eps",height = 6, width = 8)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
legend("bottomright",c("min-max pre","min-max post","z-score pre","z-score post","none"),cex=0.8,col=colors,lty=1:4)
for(i in 2:5){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
dev.off()


