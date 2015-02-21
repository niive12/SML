#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")
# library("gplots") # needs to be installed

# jsut in time... speed up
# require(compiler)
# 0 - turn off JIT
# 1 - compile closures before they are called the first time
# 2 - same as 1, plus compile closures before duplicating (useful for packages that store closures in lists, like lattice)
# 3 - same as 2, plus compile all for(), while(), and repeat() loops before executing.
# enableJIT(3) # does this even work!?!?!...

#-------------------------------------------------------------
#Functions to be used in evaluation
#-------------------------------------------------------------
source("given_functions.R")

write.latex <- function(data, col, row, filename) {
	colnames(data) = col;
	colnames(data)[1] = paste(c("&",col[1]),collapse="")
	rownames(data) = row;
	rownames(data)[1] = paste(c("\\hline\n",row[1]),collapse="")
	write.table(data, file=filename, append = FALSE, sep="\t& ", eol=" \\\\\n", col.names = TRUE, row.names = TRUE, quote = FALSE)
}


# generates a set of dataelements where: [[set]][training/testing (1/2)][number ('0','1',..)][trial, pixel]
# input data to split in format [[number ('0','1',..)][trial, pixel], split ratio = percentage of set being in training set, sets = number of different sets.
getSystematicSplit <- function(data, splitRatio, sets) { 
	noTrials = dim(data[[1]])[1] # number of ciphers in the dataset gathered (400 ciphers)
	sizeOfCipher = dim(data[[1]])[2]
	
	print(paste(c("Trials: ",noTrials),collapse=""))
	print(paste(c("Cipher: ",sizeOfCipher),collapse=""))
	
	forDataSet = round(noTrials*splitRatio) # number of ciphers for the training part
	forTestSet = (noTrials - forDataSet)
	
	result1 <- array(,c(sets,10,forDataSet,sizeOfCipher))
	result2 <- array(,c(sets,10,forTestSet,sizeOfCipher))
	
	print(paste(c("train / test: ", forDataSet, " / ", forTestSet),collapse=""))
	
	setSize = round(noTrials/sets)
	
	for(set in 1:sets)
	{
		training = array(,c(10,forDataSet,sizeOfCipher))
		testing = array(,c(10,forTestSet,sizeOfCipher))
		
		BP_start = ((sets-set)*setSize) #break points in data set
		BP_end = BP_start + forTestSet
		
		BP_foldover = BP_end - noTrials
		
		for(cipher in 1:10)
		{
			if(BP_foldover > 0)
			{
				testing[cipher,1:BP_foldover,] <- data[[cipher]][1:BP_foldover,]
				training[cipher,1:forDataSet,] <- data[[cipher]][(BP_foldover+1):BP_start,]
				if((BP_foldover) < forTestSet)
				{
					testing[cipher,(BP_foldover+1):forTestSet,] <- data[[cipher]][(BP_start+1):noTrials,]
				}
			}
			else
			{
				training[cipher,1:BP_start,] <- data[[cipher]][1:BP_start,]
				testing[cipher,1:forTestSet,] <- data[[cipher]][(BP_start+1):BP_end,]
				if((BP_start) < forDataSet)
				{
					training[cipher,(BP_start+1):(forDataSet),] <- data[[cipher]][(BP_end+1):(noTrials),]
				}
			}
		}
		result1[set,,,] <- training[]
		result2[set,,,] <- testing[]
		
	}
	return(list(t1=result1,t2=result2))
}

KNN_test_one_person <- function(d, s, g, m, k, noRuns=1, filename="none", smooth="none", sigma=1){
	DPI = c(100,200,300)
	
	# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
	RawTrainData1 = loadSinglePersonsData(DPI[[d]],g,m,smooth,sigma);
	
	# print("data aquired.")
	
	# split data
	# print("Spliting dataset...")
	splitList = getSystematicSplit(RawTrainData1,s,noRuns)
	trained <- splitList$t1
	tested <- splitList$t2
	# print("Dataset split.")
	
	timing = proc.time()
	
	#run program
	new_data = percentageDetected(tested, trained, k, filename);
	mean_data = new_data$mean
	var_data  = new_data$var
	
	timing = proc.time() - timing;
	timing = timing[["elapsed"]]
	print(paste(c("mean: ",mean_data),collapse=""))
	print(paste(c("var: ",var_data),collapse=""))
	print(paste(c("\tTime: ",timing),collapse=""))
	# 	new_data = append(timing,new_data,after=length(timing))
	return(list(time=timing,mean=mean_data,var=var_data))
	#write down data
	# 	data_file = read.csv(file=paste(c("result_G",g,"M",m,".csv"),collapse=""))
	# 	name = paste(c("K",k,"_S",s,"_D",DPI[d]),collapse="")
	# 	data_file$name = new_data;
	
	# 	print(new_data)
	# 	write.csv(data_file, file=paste(c("result_G",g,"M",m,".csv"),collapse=""),row.names=FALSE)
}

# calculates the char it is expected to be using KNN
# test vector is a single vector that represents the chracter
# trainVectors are the vectors [char type (1-10),element (the train chars stored),pixels]
# k is the number of neighbours taken into account, is a vector
KNN <- function(testVector, trainVectors, k){ 
	noOfChars <- dim(trainVectors)[1]
	noOfElements <- dim(trainVectors)[2]
	noOfDimensions <- dim(trainVectors)[3]
	noK <- length(k)
	maxK <- max(k)
	#   print(c("size of k: ",noK, " max k: ",maxK))
	
	neighbours <- array(Inf,c(maxK,2)) # used to store: (dist,char) for knn, default = Infinity
	testDistance <- array(,c(2,noOfDimensions))
	testDistance[1,] <- testVector[] # laod the testvector into the matrix used to compute distance
	
	for(char in 1:noOfChars){
		for(element in 1:noOfElements){
			# for each element in each char compute the distance
			testDistance[2,] <- trainVectors[char,element,]
			distance <- dist(testDistance,method = "euclidean")
			# if the distance is less than those already stored, add it (dist, char)
			if(neighbours[maxK,1] > distance){
				neighbours[maxK,] <- c(distance,char)
				i <- (maxK-1)
				while(neighbours[i,1] > distance && i > 0){
					neighbours[i+1,] <- neighbours[i,]
					neighbours[i,] <- c(distance,char)
					i <- (i-1)
				}
			}
		}
	}
	
	# determine the result (most voted) (bucket sort)
	kreturne <- 1:noK
	for(kval in 1:noK){
		voteing <- array(0,c(noOfChars)) # used for calculating votes using bucket sort
		for(sort in 1:k[kval]){
			elementVotedFor <- neighbours[sort,2] # (values from 1 to noOfChars)
			if(elementVotedFor != Inf){
				voteing[elementVotedFor] <- (voteing[elementVotedFor] + 1)
			}
		}
		
		# find the most voted
		result <- 1
		for(j in 2:noOfChars){
			if(voteing[result] < voteing[j]){
				result <-j
			}
		}
		
		kreturne[kval] <- result
	}
	
	return(kreturne)
}

percentageDetected   <- function(testData, trainData,k, filename="none"){
	testSets = dim(testData)[1];
	testChars = dim(testData)[2];
	testElements = dim(testData)[3];
	noK = length(k)
	# 	print(c("size of k: ",noK))
	
	percentageVec = array(,c(noK));
	varianceVec   = array(,c(noK));
	
	confus = array(0,c(noK,testChars,testChars)) # k, char, cunfused.
	for(testSet in 1:testSets){
		# do for each testset
		for(char in 1:testChars){
			for(element in 1:testElements){
				result = KNN(testData[testSet,char,element,],trainData[testSet,,,], k) #number of neighbours
				# a vector is returned...
				# 				if(result == char){ # does not work with k a vector
				# 					trueDetections = (trueDetections + 1)
				# 				}
				# 				confus[result,char] = confus[result,char]+1
				for(kRes in 1:noK){
					confus[kRes,result[kRes],char] = confus[kRes,result[kRes],char] + 1
				}
			}
		}
	}
	
# 	for(perK in 1:noK){ # count true detections (the diagonal) and find percentage
# 		trueDetections = 0
# 		for(i in 1:testChars){
# 			trueDetections <- (trueDetections + confus[perK,i,i])
# 		}
# 		percentageVec[perK] = (trueDetections/(testChars*testElements*testSets))
# 	}
	for(perK in 1:noK){ # count true detections (the diagonal) and find percentage
		trueDetections = array(,testChars)
		for(i in 1:testChars){
			trueDetections[i] <- (confus[perK,i,i]/(testElements*testSets))
		}
		percentageVec[perK] = mean(trueDetections)
		varianceVec[perK]   =  var(trueDetections)
	}
	if(filename != "none"){
		for(i in 1:noK){
			name = paste(c(filename,"_",i,".tex"),collapse="");
			write.latex(confus[i,,], 0:9, 0:9, name);
		}
	}
	return(list(mean=percentageVec,var=varianceVec));
}

use_input <- function() {
	args <- commandArgs(trailingOnly =TRUE)
	if(length(args) == 5 ){
		good_input = TRUE;
		d = as.numeric(args[1]); # DPI
		s = as.numeric(args[2]); #training part of split
		g = as.numeric(args[3]); #group number
		m = as.numeric(args[4]); #group member
		k = as.numeric(args[5]); #K
		if(d > 3) {
			good_input = FALSE; print("bad DPI")
		}
		if(s < 1) {
			st = 1-s;
		} else {
			good_input = FALSE; print("bad split")
		}
		if( g == 1 || g == 2 || g == 4 || g == 7 ){
			activeGroupNr = as.numeric(args[2])
			#group member
			if(m > 3){
				good_input = FALSE; print("bad member")
			}
		} else if( g == 3 || g == 5 || g == 6 ){
			if(m > 2){
				good_input = FALSE; print("bad member")
			}
		} else {
			good_input = FALSE; print("bad group")
		}
		
		if (k <= 1 ) {
			good_input = FALSE; print("bad K")
		}
		
		if ( good_input == TRUE ) {
			KNN_test_one_person(d, s, g, m, k);
		} else {
			print("bad input")
		}
	} else {
		print("The input format is: 'DPI','Split','Group number','member number','K'")
	}
}

# generates data for a contour plot
# k and train are vectors with the vlaues wanted to test
getContours <- function(kVlues, trainValues,testTrials,g,m){ 
	sizeK = length(kVlues)
	sizeT = length(trainValues)
	
	testSize = 40 # no of elements in the test array
	
	DPI = c(100,200,300)
	# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter

	RawTrainData = loadSinglePersonsData(DPI[[1]],g,m);
	
	
	resultContour <- array(0,c(sizeT,sizeK))
	
	for(runs in 1:sizeT){
		
		# make sure that the two arrays will have the right size
		wantedDataPoints <- trainValues[runs] + testSize
		percentage <- trainValues[runs]/wantedDataPoints
		
		if(wantedDataPoints <= dim(RawTrainData[[1]])[1]){
			# 			print(c("testing",trainValues[runs],"/",wantedDataPoints))
			
			# the new data
			# 			print("spliting data..")
			tempM <- matrix(,wantedDataPoints,dim(RawTrainData[[1]])[1])
			newData <- list(1:10);
			for(i in 1:10){
				tempM <- RawTrainData[[i]][1:wantedDataPoints,]
				newData[[i]] <-tempM
			}
			# 			print("data split.")
			
			# split data
			splitList = getSystematicSplit(newData,percentage,testTrials)
			trained <- splitList$t1
			tested <- splitList$t2
			
			
			new_data = percentageDetected(tested,trained,kVlues);
			
			resultContour[runs,] <- new_data
		}
		else{
			print("ERROR: wantedDataPoints too greate.")
		}
	}
	
	setEPS()
	postscript(paste(c("graph_G",g,"M",m,".eps"),collapse=""),height = 6, width = 8)
	filled.contour(y = kVlues, x = trainValues, resultContour, col=colorpanel(20, "grey10", "white"), nlevels=20)
	title(main = NULL, xlab = "Training Array Size", ylab = "K")
	dev.off()
	filled.contour(y = kVlues, x = trainValues, resultContour, col=colorpanel(20, "grey10", "white"), nlevels=20)
	title(main = NULL, xlab = "Training Array Size", ylab = "K")
		
}

#-------------------------------------------------------------
#This is the "main function" or the code that is actualy run
#-------------------------------------------------------------


# KNN_test_one_person(1, 0.9, 3, 2, ktest,"raw.tex")
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"smooth.tex", smooth="avarage")
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"gauss1.tex", smooth="gaussian",sigma=1)
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"gauss2.tex", smooth="gaussian",sigma=2)
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"gauss3.tex", smooth="gaussian",sigma=3)
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"gauss4.tex", smooth="gaussian",sigma=4)
# KNN_test_one_person(1, 0.9, 3, 2, ktest,"gauss5.tex", smooth="gaussian",sigma=5)

# KNN_test_one_person(1, 0.5, 3, 2, 1)
# writes to csv
#use_input();
