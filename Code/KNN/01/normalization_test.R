source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

# compare one from dataset to all others (do with 10 different ppl)
# plot sucess for that person
# do with normalization before and after with both normalizations

# take plot way from: results.R
# b = streg imellem, o = streg igennem

testPerson <- c(3,2)
PCA = 0.9
k = 1

runs = 10;
split = 0.9

print("1 person at 100DPI..")
# make data
prepareAllMixedCrossVal(split,runs,peopleToLoad = list(testPerson))

result <- matrix(0,4,runs)


# finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
for(runType in 1:2){
	for(run in 1:runs){
		load("crossVal_DPI100_",split,"_FILTERnone_",run,".RData")
		
		preNormalized <- normalizeData(finalData, "min-max")
		preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
		
		postNormalized <- pca_simplification(finalData, breakpoint = PCA)
		postNormalized <- normalizeData(postNormalized, "min-max")
		
		result[((runtype-1)*2 + 1),runs] <- run_knn(preNormalized,k) # pre
		result[((runtype-1)*2 + 2),runs] <- run_knn(postNormalized,k) # post
	}
}

print(result)

mean <- 1:4

for(i in 1:4){
	mean[i] <- mean(result[i,])
}

print("mean")
print(mean)

# setEPS()
# postscript("success_vs_dpi.eps",height = 4, width = 8)
# plot(dpi,time,type="b",xlab="DPI",ylab="time [s]",ylim=(c(250,1600)),col="black")
# dev.off()



