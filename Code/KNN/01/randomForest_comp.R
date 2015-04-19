library("randomForest")

source("normalize.R")
source("load_people_data.R")
source("pca_test.R")

load = list(trainPartSize=400,testSize=400)
pca = list(breakpoint = 1)
norm = list(normMethod = "entropy",ent_divs = 200)

trees = 200
ppl = getPeople()
res = 1:length(ppl)

startTimer <- proc.time()

for(person in 1:length(ppl)){
	data = prepareOneAloneNormPCA(ppl[[person]][1], ppl[[person]][2], load, norm, pca , make_new = 1)
	
	confus = (randomForest(x = data$trainSet,y = as.factor(data$trainVali), ntree = trees, xtest = data$testSet, ytest = as.factor(data$testVali))$test)$confusion
	per = 0
	for(j in 1:10){
		per = per + confus[j,j]
	}
	per = per/length(data$testVali)
	res[person] = per
	print(paste(c(person , " / ", length(ppl), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(ppl)-person)*((proc.time() - startTimer)[3])/person), collapse = "") )
}

print(res)

x_lab <- 1:length(ppl)
for(i in 1:length(ppl)){
	x_lab[i] <- paste(c(ppl[[i]][1],":",ppl[[i]][2]),collapse="")
}


setEPS()
postscript("successRate_randomForest_comp.eps",height = 6, width = 8)

plot(1:length(ppl),res, xaxt="n",type="b", xlab="Trees", ylab="Success Rate [%]")
axis(1, at=1:length(ppl), labels=x_lab)

q = dev.off()