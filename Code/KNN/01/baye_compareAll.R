#  takes about 6h in full resolution (400 400) for 12 ppl
#  takes about 10h30 in full resolution (400 400) for 16 ppl

library("graphics")
library("parallel") 

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("Baye.R")


# compare one from dataset to all others (do with 10 different ppl)
# plot sucess for that person
# do with normalization before and after with both normalizations
# best plot: k = 19, PCA = 0.8
# prepareOneAlone(g,m, trainPartSize = 100, testSize = 40, peopleToLoad = getPeople())

# take plot way from: results.R
# b = streg imellem, o = streg igennem
startTimer <- proc.time() # used for timing

PCA = 0.5
bins = 20
laplace = 1
tresh = 0.1
eps = 0.1

train_size = 400
test_size = 400


# prep for data
people <- getPeople()
noPeople <-length(people)
result <- matrix(0,1,noPeople)

fileName <- "bayes-test_"

for(i in 1:length(people)){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")


if(file.exists(fileName) && 1){
	load(fileName)
} else{
	startTime <- proc.time() # used for timing
	# make loop to save data
	for(person in 1:noPeople){
		data <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = train_size, testSize = test_size, peopleToLoad = people)
		
		# reduce data (pca)
		data <- normalizeData(data, "z-score")
		data <- pca_simplification(data, breakpoint=PCA)
			
		# bin
		data <- normalizeData(data, "bin", bins = bins)
		
		# z-score, PCA, Bayes
# 		result[1,person] <- (baye_predict(data, laplace = laplace, threshold = tresh, eps=eps))$success
		result[1,person] <- (baye_predict(data))$success
		print(result[1,person])
		timer <- (((proc.time() - startTime)[3])*(noPeople-person)/person)
		print(paste(c(person, "/", noPeople, " datasets loaded. Estimated finish time: ",timer, " seconds."),collapse = ""))
		
		# save the data
		save(result, file = fileName)
	}
	
	print(paste(c("time taken to run program: ", (proc.time() - startTime)[3]),collapse = ""))
}

print(result)

print(mean(result[1,]))

# 0.44050 0.33925 0.47000 0.40975 0.38300 # 100 40 test K = 19
# 0.5032083 0.4072083 0.5439375 0.4913542 0.4149167 # 400 400 test K = 19
# 0.5532031 0.4493750 0.5874687 0.5339844 0.4791094 # 400 400 test 16 ppl K = 19

#  K = 10
# 0.6439500 0.5409667 0.6769833 0.6252333 0.5667167 with new load fun

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
setEPS()
postscript("graph_normalization.eps",height = 6, width = 8)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) # ylim=(c(min(result),max(result))),
axis(1, at=1:noPeople, labels=x_lab)
dev.off()
print(paste(c("Time taken to run program: ", ((proc.time() - startTimer)[3]), " seconds"), collapse = "") )
