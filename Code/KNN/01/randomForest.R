# seq(100,1000,100) = 6h40m, first 7 = 3h30m, ent div = 20
# line = c(seq(5,95,5),seq(100,1000,50)), ent_divs = 200 = 12h10m

library("randomForest")

source("normalize.R")
source("load_people_data.R")
source("pca_test.R")
load = list(trainPartSize=400,testSize=400)
pca = list(breakpoint = 1)
norm = list(normMethod = "entropy",ent_divs = 200)
data = prepareOneAloneNormPCA(3, 2, load, norm, pca , make_new = 1)

#line = seq(5,15,5)
line = c(seq(5,95,5),seq(100,1000,50))
res = 1:length(line)
time = 1:length(line)

startTimer <- proc.time()

for(i in 1:length(line)){
	timerFunc <- proc.time()
	confus = (randomForest(x = data$trainSet,y = as.factor(data$trainVali), ntree = line[i], xtest = data$testSet, ytest = as.factor(data$testVali))$test)$confusion
	per = 0
	for(j in 1:10){
		per = per + confus[j,j]
	}
	per = per/length(data$testVali)
	res[i] = per
	time[i] = (proc.time() - timerFunc)[["user.self"]]
	print(paste(c(i , " / ", length(line), ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (length(line)-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
}

print(res)

colors= c("black","blue")

setEPS()
postscript("successRate_randomForest.eps",height = 6, width = 8)

par(mar=c(5, 4, 4, 6) + 0.1)

plot(line,res, type="l",lty=1, col=colors[1],axes=FALSE, pch=16, xlab="", ylab="")
axis(2, ylim=c(0,1),col=colors[1],las=1)
mtext(paste("Success [%]",collapse=""),side=2,line=2.5)
box()

par(new=TRUE)

plot(line, time, pch=15, col=colors[2],
	 axes=FALSE, type="l", xlab="", ylab="")
mtext(paste("Time [s]",collapse=""),side=4,col=colors[2],line=4) 
axis(4, ylim=c(0,max(line)), col=colors[2],col.axis=colors[2],las=1)

# Draw the axis
axis(1,pretty(range(line),10))
mtext("Trees",side=1,col="black",line=2.5)  

legend("bottomright",legend=c("Success","Time"),pch=16:15,cex=0.8,col=colors)

q = dev.off()