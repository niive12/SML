#  takes about xxhxx in full resolution (400 400) for 16 ppl

library("graphics")

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
smooth = 0.7
smoothSize = 9


# prep for data
people <- getPeople()
noPeople <-length(people)
result <- matrix(0,3,noPeople) # none, z-score pre, z-score pre with smooth


fileName <- "norm_smooth-test_"

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
		data_s <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = 400, testSize = 400, peopleToLoad = people, make_new=1, sigma =smooth, size =smoothSize, filter = "gaussian" )
		data <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = 400, testSize = 400, peopleToLoad = people, make_new=1, filter = "none" )
		
		# 	print("no normalization")
		result[1,person] <- run_knn(data,k)$success # none
		
		# z-score
		# 	print("z score")
		preNormalized <- normalizeData(data, "z-score")
		preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)

		result[2,person] <- run_knn(preNormalized,k)$success # zscore pre
		
		# smooth z score pre
		preNormalized <- normalizeData(data_s, "z-score")
		preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
		
		result[3,person] <- run_knn(preNormalized,k)$success # zscore pre
		
		
		timer <- (((proc.time() - startTime)[3])*(noPeople-person)/person)
		print(paste(c(person, "/", noPeople, " datasets loaded. Estimated finish time: ",timer, " seconds."),collapse = ""))
		
		# save the data
		save(result, file = fileName)
	}
}


# print(result)

# mean
mean <- 1:3

for(i in 1:3){
	mean[i] <- mean(result[i,])
}

print("mean")
print(mean)

#result

#make x string
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

print(x_lab)

colors = rainbow(3)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
legend("bottomright",c("none","z-score pre","z-score smoothed"),cex=0.8,col=colors,lty=1:4)
for(i in 2:3){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
setEPS()
postscript("graph_normalization_smoothed.eps",height = 6, width = 8)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
legend("bottomright",c("none","z-score pre","z-score smoothed"),cex=0.8,col=colors,lty=1:4)
for(i in 2:3){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
dev.off()


