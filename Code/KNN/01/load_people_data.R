source("given_functions.R")

library("class") # for knn test
library("base") # for timing


# the purpose of this source code is to load in the data needed and prepare it for processing. It will then be stored in external .dat files.
# the data is prepared to be used for the KNN method in 

# working director with the data
# must be two folders into the folder with the data (in a member folder) because the loadSinglePersonsData function from given_functions currently defines it..
dataDepository = "C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/member1" # lukas
# dataDepository = "" # nikolaj


# -- misc --
# getwd()y 
# setwd()

dpi = 1    # dpi
split = 0.5 # split tEst


# list of people to access data from
getPeople <- function(){
	# people and their group
	p0 = c(1, 1)
# 	p1 = c(1, 2)
	p2 = c(1, 3)
# 	p3 = c(2, 1)
	p4 = c(2, 2)
	p5 = c(2, 3)
	p6 = c(3, 1)
	p7 = c(3, 2)
	p8 = c(4, 1)
# 	p9 = c(4, 2)
# 	p10 = c(4, 3)
	p11 = c(5, 1)
	p12 = c(5, 2)
	p13 = c(6, 1)
# 	p14 = c(6, 2)
# 	p15 = c(7, 1)
# 	p16 = c(7, 2)
# 	p17 = c(7, 3)
	all = list(p0, p2, p4, p5, p6, p7, p8, p11, p12, p13)
	# all = list(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
	# test_people  = c(p6, p7)
	# train_people = p7
	test = list(p7)
# 	test = list(p6,p7)
# 	test = list(p5,p6,p7)
# 	test = list(p4,p5,p6,p7)
# 	test = list(p2,p4,p5,p6,p7)
	
	return(test)
}


# load all group members data
loadAllPeople <- function(DPI, filter = "none"){
	# returns list( the greatest number of pixels for a cipher , a matrix with the data of all people )
	
	# get the people to load data for
	people <- getPeople()

	# number of people
	noPeople <- length(people)
	print(paste(c("Loading ", noPeople, " datasets..."),collapse=""))
	
	# place to store the data
	peopleData <- list(1:noPeople);
	
	# change the working directory to where the data is
	currentWD <- getwd()
	setwd(dataDepository)
	#print(currentWD)
	
	# the greatest number of pixels for a cipher 
	maxCipherSize <- 0
	
	# load all the people
	for(person in 1:noPeople){
		print(paste(c(" - Loading G", people[[person]][1], "M", people[[person]][2],"..."),collapse=""))
		peopleData[[person]] <- loadSinglePersonsData(DPI,people[[person]][1],people[[person]][2], filter)
		
		# find the biggest size of a cipher
		sizeOfCipher <- dim(peopleData[[person]][[1]])[2]
		if(sizeOfCipher > maxCipherSize){
			maxCipherSize <- sizeOfCipher
		}
	}
	
	setwd(currentWD)
	
	
	return(list(cipherSize=maxCipherSize, data=peopleData))
}


prepareAllMixed <- function(trainPart,testPart, DPI = 100 , filter = "none"){ # the number of elements (ciphers) taken from each group
	
	if(trainPart+testPart > 400){
		e <- simpleError(paste(c("Bad input. Too high train + test part: ",trainPart+testPart, " > 400."),collapse=""))
		stop(e)
		print("test")
	}
	
	# load the data
	print("Loading data...")
	dataResult = loadAllPeople(DPI, filter)
	print("Data loaded.")
	data <- dataResult$data
	maxCipher <- dataResult$cipherSize
	noPeople <- length(data)
	
	# define different parameters
	ciphersPerChar <- 400
	
	# number of elements taken from one person for the train and test sets
	noPersonTrain <- trainPart * 10
	noPersonTest <- testPart * 10
	
	# number of elements in total
	noTotalTrain <- noPersonTrain * noPeople
	noTotalTest <- noPersonTest * noPeople
	
	train <- matrix( 0 , nrow=noTotalTrain , ncol=maxCipher) 
	test <- matrix( 0 , nrow=noTotalTest , ncol=maxCipher) 
	train_actual <- 1:noTotalTrain
	test_actual <- 1:noTotalTest
		
		
	# prepare the test and training set
	print("Preparing training and test set...")
	for(person in 1:noPeople){
		for(cipher in 1:10){
			sizeOfCipher <- dim(data[[person]][[cipher]])[2]
			# train
			train[(((person - 1) * noPersonTrain) + ((cipher - 1) * trainPart) + 1):((person - 1) * noPersonTrain + cipher * trainPart),1:sizeOfCipher] <- data[[person]][[cipher]][1:trainPart,]
			train_actual[(((person - 1) * noPersonTrain) + ((cipher - 1) * trainPart) + 1):((person - 1) * noPersonTrain + cipher * trainPart)] <- rep.int(cipher,trainPart)
			#test
			test[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart),1:sizeOfCipher] <- data[[person]][[cipher]][1:testPart,]
			test_actual[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart)] <- rep.int(cipher,testPart)
		}
	}
	print("Preparation of training and test set done.Saving data...")

	# save the data
	finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
	
	fileName <- paste(c("allPeople_DPI",DPI,"_",trainPart,"-",testPart,"_FILTER",filter,".RData"),collapse="")
	
	save(finalData, file = fileName)

	return(finalData) # return to test on it
}


prepareOneAlone <- function(group, member){
	
}


# -- test run knn --
# prepare data
currentTime <- proc.time()
haha <- prepareAllMixed(200,200) # default: 100 dpi and no filter
print(paste(c("Time taken to load: ",(proc.time() - currentTime)[1]),collapse=""))

# calc KNN
# currentTime <- proc.time()
# r = knn(haha$trainSet,haha$testSet,haha$trainVali,10)
# print(paste(c("Time taken to run KNN: ", (proc.time() - currentTime)[1]),collapse=""))
# 
# # count right detections
# per <- 0
# for(i in 1:length(haha$testVali)){
# 	if(r[i] == (haha$testVali)[i]){
# 		per <- per + 1
# 	}
# }
# 
# print(per/length(haha$testVali))
