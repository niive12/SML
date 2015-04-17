library("randomForest")

source("normalize.R")
source("load_people_data.R")
source("pca_test.R")

load = list(trainPartSize=10,testSize=10)
pca = list(breakpoint = 1)
norm = list(normMethod = "entropy",ent_divs = 20)

trees = 10
ppl = getPeople()

startTimer <- proc.time()

for(person in 1:length(ppl)){
	data = prepareOneAloneNormPCA(ppl[[person]][1], ppl[[person]][2], load, norm, pca , make_new = 0)
	
	confus = (randomForest(x = data$trainSet,y = as.factor(data$trainVali), ntree = trees, xtest = data$testSet, ytest = as.factor(data$testVali))$test)$confusion
	per = 0
	for(j in 1:10){
		per = per + confus[j,j]
	}
	per = per/length(data$testVali)
	res[i] = per
	print(paste(c(i , " / ", length(line), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(line)-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
}

print(res)

setEPS()
postscript("successRate_randomForest_comp.eps",height = 6, width = 8)

plot(line,res, type="l", xlab="Trees", ylab="Success [%]")

q = dev.off()