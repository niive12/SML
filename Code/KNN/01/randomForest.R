# seq(100,1000,100) = 6h40m, first 7 = 3h30m, ent div = 20
# line = c(seq(5,95,5),seq(100,1000,50)), ent_divs = 200 = 12h10m
# 29h

library("randomForest")

source("normalize.R")
source("load_people_data.R")
source("pca_test.R")

#line = seq(5,15,5)
line = c(seq(5,95,5),seq(100,1000,50))
res = matrix(,2,length(line))
time = matrix(,2,length(line))

filename = "rnadomForest_time.RData"

if(T){
	load = list(trainPartSize=400,testSize=400)
	pca = list(breakpoint = 1)
	norm_e = list(normMethod = "entropy",ent_divs = 200)
	norm_n = list(normMethod = "z-score")
	data_e = prepareOneAloneNormPCA(3, 2, load, norm_e, pca , make_new = 1)
	data_n = prepareOneAloneNormPCA(3, 2, load, norm_n, pca , make_new = 1)
	
	startTimer <- proc.time()
	
	for(i in 1:length(line)){
		timerFunc <- proc.time()
		confus = (randomForest(x = data_e$trainSet,y = as.factor(data_e$trainVali), ntree = line[i], xtest = data_e$testSet, ytest = as.factor(data_e$testVali))$test)$confusion
		per = 0
		for(j in 1:10){
			per = per + confus[j,j]
		}
		per = per/length(data_e$testVali)
		res[1,i] = per
		time[1,i] = (proc.time() - timerFunc)[["user.self"]]
		print(paste(c(i , " / ", length(line), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(line)-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
	}
	
	for(i in 1:length(line)){
		timerFunc <- proc.time()
		confus = (randomForest(x = data_n$trainSet,y = as.factor(data_n$trainVali), ntree = line[i], xtest = data_n$testSet, ytest = as.factor(data_n$testVali))$test)$confusion
		per = 0
		for(j in 1:10){
			per = per + confus[j,j]
		}
		per = per/length(data_n$testVali)
		res[2,i] = per
		time[2,i] = (proc.time() - timerFunc)[["user.self"]]
		print(paste(c(i , " / ", length(line), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(line)-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
	}
	
	print(res)

	print(paste(c("mean e: ", mean(res[1,])),collapse=""))
	
	print(paste(c("mean n: ", mean(res[2,])),collapse=""))
	
	save(res, time, file = filename)
}else{
	load(filename)	
}

colors = rainbow(4)

setEPS()
postscript("successRate_randomForest.eps",height = 6, width = 8)

par(mar=c(5, 4, 4, 6) + 0.1)

plot(line,res[1,], type="l",lty=1, col=colors[1],axes=FALSE, pch=16, xlab="", ylab="", ylim=c(min(res),max(res)))
lines(line,res[2,],type="l",lty=1, col=colors[2])
axis(2,col="black",las=1)
mtext(paste("Success [%]",collapse=""),side=2,line=2.5)
box()

par(new=TRUE)

plot(line, time[1,], pch=15, col=colors[3],
	 axes=FALSE, type="l", xlab="", ylab="", ylim=c(0,max(time[])))
lines(line,time[2,],type="l",lty=1, col=colors[4])
mtext(paste("Time [s]",collapse=""),side=4,col="black",line=4) 
axis(4, col="black",col.axis="black",las=1)

# Draw the axis
axis(1,pretty(range(line),10))
mtext("Trees",side=1,col="black",line=2.5)  

legend("bottomright",legend=c("Success Ent.","Success N.","Time Ent.","Time N."),pch=16:13,cex=0.8,col=colors)

q = dev.off()