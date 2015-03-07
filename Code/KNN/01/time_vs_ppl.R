# library("gplots") # needs to be installe
library("graphics")

source("load_people_data.R")
source("pca_test.R")



# all-mixed (100 DPI)
# time vs no. ppl. (90/10 split, fixed K = 10)

k = 10
split = 0.9

people <- getPeople()
noPeople <- length(people)

result <- matrix(0,4,noPeople-1)

# make unique filename
fileName <- "timeVSppl-test"

for(i in 1:noPeople){
	fileName <- paste(c(fileName,"_G",people[[i]][1],"M",people[[i]][2]),collapse="")
}

fileName <- paste(c(fileName,".RData"),collapse="")


# run test
if(file.exists(fileName) && 0){
	load(fileName)
} else{
	for(ppl in 2:noPeople){
		
		# load dataset
		data <- prepareOneAlone(people[[1]][1],people[[1]][2], trainPartSize = 400, testSize = 100, peopleToLoad = people[1:ppl])
		
		# time prediction
		startTime <- proc.time() # used for timing
		r <-run_knn(data,k)$success # zscore pre
		time <- ((proc.time() - startTime)[3])
		
		# 	save the data
		result[1,ppl-1] <- time # time (sec)
		result[2,ppl-1] <- 100*10 # test size
		result[3,ppl-1] <- (400)*(ppl-1)*10 # train size
		result[4,ppl-1] <- result[1,(ppl-1)]/result[2,(ppl-1)] # time/test
		
		save(result, file = fileName)
	}
	
	
}

# plot
plot(result[3,],result[4,],type="b",xlab="Number of Entries in the Training Set",ylab="Time per Prediction (sec)") 

setEPS()
postscript("graph_timeVSppl.eps",height = 6, width = 8)
plot(result[3,],result[4,], type="b",xlab="Number of Entries in the Training Set",ylab="Time per Prediction (sec)") 
dev.off()
