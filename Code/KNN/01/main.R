#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")

# jsut in time... speed up
require(compiler)
# 0 - turn off JIT
# 1 - compile closures before they are called the first time
# 2 - same as 1, plus compile closures before duplicating (useful for packages that store closures in lists, like lattice)
# 3 - same as 2, plus compile all for(), while(), and repeat() loops before executing.
enableJIT(3)

#-------------------------------------------------------------
#Functions to be used in evaluation
#-------------------------------------------------------------
source("given_functions.R")



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

KNN_test_one_person <- function(d, s, g, m, k){
	DPI = c(100,200,300)

	# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
	RawTrainData1 = loadSinglePersonsData(DPI[[1]],g,m);
	
	# print("data aquired.")

	# split data
	# print("Spliting dataset...")
	splitList = getSystematicSplit(RawTrainData1,s,1)
	trained <- splitList$t1
	tested <- splitList$t2
	# print("Dataset split.")

	timing = proc.time()
	
	#run program
	new_data = percentageDetected(tested,trained);
	
	timing = proc.time() - timing;
	timing = timing[["elapsed"]]
	new_data = append(timing,new_data,after=length(timing))
# 		
# 	#write down data
# 	file=paste(c("result_G",g,"M",m,".csv"),collapse="")
# 	if(!(file.exists(file))){
# 		file.create(file)
# 	}
# 	
# 	data_file = try(read.csv(file),silent=true)
# 	name = paste(c("K",k,"_S",s,"_D",DPI[d]),collapse="")
# 	data_file$name = new_data;
# 	write.csv(data_file, file, row.names=FALSE)

}

# calculates the char it is expected to be using KNN
# test vector is a single vector that represents the chracter
# trainVectors are the vectors [char type (1-10),element (the train chars stored),pixels]
# k is the number of neighbours taken into account
KNN <- function(testVector, trainVectors,k){ 
  noOfChars <- dim(trainVectors)[1]
  noOfElements <- dim(trainVectors)[2]
  noOfDimensions <- dim(trainVectors)[3]
  
  voteing <- array(0,c(noOfChars)) # used for calculating votes using bucket sort
  neighbours <- array(Inf,c(k,2)) # used to store: (dist,char) for knn, default = Infinity
  testDistance <- array(,c(2,noOfDimensions))
  testDistance[1,] <- testVector[] # laod the testvector into the matrix used to compute distance
  
  for(char in 1:noOfChars){
    for(element in 1:noOfElements){
      # for each element in each char compute the distance
      testDistance[2,] <- trainVectors[char,element,]
      distance <- dist(testDistance,method = "euclidean")
      # if the distance is less than those already stored, add it (dist, char)
      if(neighbours[k,1] > distance){
        neighbours[k,] <- c(distance,char)
        i <- (k-1)
        while(neighbours[i,1] > distance && i > 0){
          neighbours[i+1,] <- neighbours[i,]
          neighbours[i,] <- c(distance,char)
          i <- (i-1)
        }
      }
    }
  }
  
  # determine the result (most voted) (bucket sort)
  for(sort in 1:k){
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
  
  return(result)
}

percentageDetected   <- function(testData, trainData){
	testSets = dim(testData)[1];
	testChars = dim(testData)[2];
	testElements = dim(testData)[3];
	
	percentageVec = array(,testSets);

	for(testSet in 1:testSets){
		
		trueDetections = 0;

		# do for each testset
		for(char in 1:testChars){
			for(element in 1:testElements){
				result = KNN(testData[testSet,char,element,],trainData[testSet,,,],1) #number of neighbours
				if(result == char){
					trueDetections = (trueDetections + 1)
				}
			}
		}
		percentageVec[[testSet]] = trueDetections/(testChars*testElements)
	}
	return(percentageVec);
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
		if( g == 1 || g == 2 || g == 4 || g == 5 || g == 6 || g == 7 || g == 8 ){
			activeGroupNr = as.numeric(args[2])
			#group member
			if(m > 3){
				good_input = FALSE; print("bad member")
			}
		} else if( g == 3 || g == 9 ){
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

#-------------------------------------------------------------
#This is the "main function" or the code that is actualy run
#-------------------------------------------------------------
#get data from png images: 


#                   dpi split group member k
KNN_test_one_person(1, 0.5, 3, 2, 1);
# writes to csv
#use_input();

