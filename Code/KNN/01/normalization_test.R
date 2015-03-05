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
k = 19



# prep for data
people <- getPeople()
noPeople <-length(people)
result <- matrix(0,4,noPeople)


# make loop to save data
for(person in 1:noPeople){
	data <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = 100, testSize = 40, peopleToLoad = people)
	
	# min max
	preNormalized <- normalizeData(data, "min-max")
	preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
	
	postNormalized <- pca_simplification(data, breakpoint = PCA)
	postNormalized <- normalizeData(postNormalized, "min-max")
	
	result[1,person] <- run_knn(preNormalized,k)$success # pre
	result[2,person] <- run_knn(postNormalized,k)$success # post
	
	# z-score
	preNormalized <- normalizeData(data, "z-score")
	preNormalized <- pca_simplification(preNormalized, breakpoint = PCA)
	
	postNormalized <- pca_simplification(data, breakpoint = PCA)
	postNormalized <- normalizeData(postNormalized, "z-score")
	
	result[3,person] <- run_knn(preNormalized,k)$success # pre
	result[4,person] <- run_knn(postNormalized,k)$success # post
	
	print(paste(c(person, "/", noPeople, " datasets loaded."),collapse = ""))
}



print(result)

# mean
mean <- 1:4

for(i in 1:4){
	mean[i] <- mean(result[i,])
}

print("mean")
print(mean)
#  0.43975 0.34050 0.46850 0.40775


#result

#make x string
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

print(x_lab)

colors = rainbow(4)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
# legend("topright",c("min-max pre","min-max post","z-score pre","z-score post"), fill=colors, horiz=TRUE)
legend("bottomright",c("min-max pre","min-max post","z-score pre","z-score post"),cex=0.8,col=colors,lty=1:4)
for(i in 2:4){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
setEPS()
postscript("graph.eps",height = 6, width = 8)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
# legend("topright",c("min-max pre","min-max post","z-score pre","z-score post"), fill=colors, horiz=TRUE)
legend("bottomright",c("min-max pre","min-max post","z-score pre","z-score post"),cex=0.8,col=colors,lty=1:4)
for(i in 2:4){
	lines(1:noPeople,result[i,],type="b",lty=i, col=colors[i])
}
dev.off()


