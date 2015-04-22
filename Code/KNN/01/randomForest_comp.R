# 17h
library("randomForest")

source("normalize.R")
source("load_people_data.R")
source("pca_test.R")

load = list(trainPartSize=400,testSize=400)
pca = list(breakpoint = 1)
norm_e = list(normMethod = "entropy",ent_divs = 200)
norm_n = list(normMethod = "z-score")

trees = 200
ppl = getPeople()
res = matrix(,2,length(ppl))

filename = "randomForest_comp.RData"

if(F){
	startTimer <- proc.time()
	
	for(person in 1:length(ppl)){
		data = prepareOneAloneNormPCA(ppl[[person]][1], ppl[[person]][2], load, norm_e, pca , make_new = 1)
		
		confus = (randomForest(x = data$trainSet,y = as.factor(data$trainVali), ntree = trees, xtest = data$testSet, ytest = as.factor(data$testVali))$test)$confusion
		per = 0
		for(j in 1:10){
			per = per + confus[j,j]
		}
		per = per/length(data$testVali)
		res[1,person] = per
		print(paste(c(person , " / ", length(ppl), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(ppl)-person)*((proc.time() - startTimer)[3])/person), collapse = "") )
	}
	
	for(person in 1:length(ppl)){
		data = prepareOneAloneNormPCA(ppl[[person]][1], ppl[[person]][2], load, norm_n, pca , make_new = 1)
		
		confus = (randomForest(x = data$trainSet,y = as.factor(data$trainVali), ntree = trees, xtest = data$testSet, ytest = as.factor(data$testVali))$test)$confusion
		per = 0
		for(j in 1:10){
			per = per + confus[j,j]
		}
		per = per/length(data$testVali)
		res[2,person] = per
		print(paste(c(person , " / ", length(ppl), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(ppl)-person)*((proc.time() - startTimer)[3])/person), collapse = "") )
	}
	
	
	print(res)
	
	print(paste(c("mean e: ", mean(res[1,])),collapse=""))
	
	print(paste(c("mean n: ", mean(res[2,])),collapse=""))
	
	save(res, file = filename)
}else {
	load(filename)
}

x_lab <- 1:length(ppl)
for(i in 1:length(ppl)){
	x_lab[i] <- paste(c(ppl[[i]][1],":",ppl[[i]][2]),collapse="")
}

colors = rainbow(2)
setEPS()
postscript("successRate_randomForest_comp.eps",height = 6, width = 8)

plot(1:length(ppl),res[1,], xaxt="n",type="b", xlab="Person", ylab="Success Rate [%]", col = colors[1], ylim=c(min(res),max(res)))
lines(1:length(ppl),res[2,],type="b",lty=1, col=colors[2])
axis(1, at=1:length(ppl), labels=x_lab)
legend("topright",legend=c("Success Entropy","Success Z-score"),pch=16:15,cex=0.8,col=colors)

q = dev.off()