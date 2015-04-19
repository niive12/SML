source("C50predict.R")
source("load_people_data.R")
source("pca_test.R")
source("normalize.R")

library("graphics")
library("parallel") 

# compare one from dataset to all others (do with 16 different ppl)

startTimer <- proc.time() # used for timing

nPC = 50
ntrials = 15

train_size = 400
test_size = 400

# prep for data
people <- getPeople()
noPeople <-length(people)
result <- matrix(0,1,noPeople)

fileName <- "tree-test_"

for(i in 1:length(people)){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")

load_setting      = list(trainPartSize = train_size, testSize = test_size, peopleToLoad = people)
normalize_setting = list(normMethod = "z-score")
pca_setting       = list(noPC=50)

if(file.exists(fileName) && 0){
	load(fileName)
} else {
	startTime <- proc.time() # used for timing
	# make loop to save data
	for(person in 1:noPeople){
		#get data
		data = prepareOneAloneNormPCA(people[[person]][1],people[[person]][2],load_setting,normalize_setting,pca_setting)
# 		data <- prepareOneAlone(people[[person]][1],people[[person]][2], trainPartSize = train_size, testSize = test_size, peopleToLoad = people)
# 		data <- normalizeData(data, "z-score")
# 		data <- pca_simplification(data, noPC=nPC)
		
		#create model
		model = C50::C5.0(data$trainSet, as.factor(data$trainVali), trials=ntrials)
		result[1,person] = random_forrest_predict(data=data, model=model)$success
		
		timer <- (((proc.time() - startTime)[["user.self"]])*(noPeople-person)/person)
		print(result[1,person])
		print(paste(c(person, "/", noPeople, " datasets loaded. Estimated finish time: ",timer, " seconds."),collapse = ""))
		
		# save the data
		save(result, file = fileName)
	}
	
	print(paste(c("time taken to run program: ", (proc.time() - startTime)[["user.self"]]),collapse = ""))
}
#result

#make x string
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

print(c("mean", mean(result)))

colors = rainbow(5)
setEPS()
postscript("../../../Report/graphics/tree_success_all.eps",height = 6, width = 8)
plot(1:noPeople,result[1,], xaxt="n",type="b",xlab="Person",ylab="Success Rate",ylim=(c(min(result),max(result))),col=colors[1]) 
axis(1, at=1:noPeople, labels=x_lab)
dev.off()
print(paste(c("Time taken to run program: ", ((proc.time() - startTimer)[["user.self"]]), " seconds"), collapse = "") )