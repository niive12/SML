source("given_functions.R")

library("class") # for knn test
library("base") # for timing


# the purpose of this source code is to load in the data needed and prepare it for processing. It will then be stored in external .dat files.
# the data is prepared to be used for the KNN method in 



# list of people to access data from
getPeople <- function(){
	# people and their group
	p0 = c(1, 1)
	p1 = c(1, 2)
	p2 = c(1, 3)
	p3 = c(2, 1) 
	p4 = c(2, 2)
	p5 = c(2, 3)
	p6 = c(3, 1)
	p7 = c(3, 2)
	p8 = c(4, 1)
	p9 = c(4, 2)
	p10 = c(4, 3)
	p11 = c(5, 1)
	p12 = c(5, 2)
	p13 = c(6, 1)
	p14 = c(6, 2)
	p15 = c(7, 1)
# 	p16 = c(7, 2) # not at all
	p17 = c(7, 3)
	all = list(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p15, p17)
# 	all = list(p14, p6)
# all = list(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
	# test_people  = c(p6, p7)
	# train_people = p7
	
	return(all)
}


# load all group members data
loadAllPeople <- function(DPI, filter = "none", peopleToLoad, sigma=0.5, size = 5){
	# returns list( the greatest number of pixels for a cipher , a matrix with the data of all people )
	
	# get the people to load data for
	people <- peopleToLoad

	# number of people
	noPeople <- length(people)
	print(paste(c("Loading ", noPeople, " datasets..."),collapse=""))
	
	# place to store the data
	peopleData <- list(1:noPeople);
	
	# the greatest number of pixels for a cipher 
	maxCipherSize <- 0
	
	# load all the people
	startTime <- proc.time() # used for timing
	for(person in 1:noPeople){
# 		print(paste(c(" - Loading G", people[[person]][1], "M", people[[person]][2],"..."),collapse=""))
		peopleData[[person]] <- loadSinglePersonsData(DPI,people[[person]][1],people[[person]][2], filter, sigma=sigma, size=size)
		
# 		peopleData[[person]] [is.na(peopleData[[person]])] <- 0 # set NA's from dataset to zero...
		
		# find the biggest size of a cipher
		sizeOfCipher <- dim(peopleData[[person]][[1]])[2]
		if(sizeOfCipher > maxCipherSize){
			maxCipherSize <- sizeOfCipher
		}
		# timer :D
		if(person != noPeople){
			timer <- (((proc.time() - startTime)[3])*(noPeople-person)/person)
			print(paste(c("Estimated finish time of loading datasets within: ",timer, " seconds."),collapse=""))
		}
	}	
	
	return(list(cipherSize=maxCipherSize, data=peopleData))
}


prepareOne <- function(group, member, trainPart,testPart, DPI = 100 , filter = "none", make_new=0, sigma =0.5, size =5){ # the number of elements (ciphers) taken from each group
	
	fileName <- paste(c("person_G",group,"M",member,"_",DPI,"_",trainPart,"-",testPart,"_FILTER",filter,".RData"),collapse="")
	
	if ( file.exists(fileName) && !make_new) {
		print("File exist")
		load(fileName)
	} else {
		if(trainPart+testPart > 400){
			e <- simpleError(paste(c("Bad input. Too high train + test part: ",trainPart+testPart, " > 400."),collapse=""))
			stop(e)
		}
		
		# load the data
		print("Loading data...")
		personData <- list(1);
		
		personData[[1]] = loadSinglePersonsData(DPI,group,member, filter, sigma=sigma, size=size)
		maxCipherSize = dim(personData[[1]][[1]])[2]
		
		dataResult = list(cipherSize=maxCipherSize, data=personData)
		
		
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
				test[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart),1:sizeOfCipher] <- data[[person]][[cipher]][(trainPart+1):(trainPart+testPart),]
				test_actual[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart)] <- rep.int(cipher,testPart)
			}
		}
		print("Preparation of training and test set done.Saving data...")

		# save the data
		finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
		
		
		save(finalData, file = fileName)
	}
	return(finalData) # return to test on it
}

prepareAllMixed <- function(trainPart,testPart, DPI = 100 , filter = "none", peopleToLoad = getPeople(), make_new=0, sigma =0.5, size =5){ # the number of elements (ciphers) taken from each group	
	fileName <- paste(c("allPeople_DPI",DPI,"_",trainPart,"-",testPart,"_FILTER",filter),collapse="")
	
	for(i in 1:length(peopleToLoad)){
		fileName <- paste(c(fileName,"_G",peopleToLoad[[i]][1],"M",peopleToLoad[[i]][2]),collapse="")
	}
	fileName <- paste(c(fileName,".RData"),collapse="")
	
	if ( file.exists(fileName) && !make_new ) {
		print("File exist")
		load(fileName)
	} else {
		if(trainPart+testPart > 400){
			e <- simpleError(paste(c("Bad input. Too high train + test part: ",trainPart+testPart, " > 400."),collapse=""))
			stop(e)
		}
		
		# load the data
		print("Loading data...")
		dataResult = loadAllPeople(DPI, filter, peopleToLoad, sigma=sigma, size=size)
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
				test[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart),1:sizeOfCipher] <- data[[person]][[cipher]][(trainPart+1):(trainPart+testPart),]
				test_actual[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart)] <- rep.int(cipher,testPart)
			}
		}
		print("Preparation of training and test set done.Saving data...")

		# save the data
		finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
		
		
		save(finalData, file = fileName)
		
		print("Data saved.")
	}
	return(finalData) # return to test on it
}


# trainPartSize and testSize is the number of elements of one class taken from that class into on of the two sets
prepareOneAlone <- function(group, member, trainPartSize=400, testSize=200, DPI=100 , filter="none", peopleToLoad = getPeople(), make_new=0, sigma =0.5, size =5 ){
	
	fileName <- paste(c("onePerson_DPI",DPI,"_G",group,"M",member,"_SIZE",trainPartSize,"-",testSize,"_FILTER",filter),collapse="")
	
	for(i in 1:length(peopleToLoad)){
		fileName <- paste(c(fileName,"_G",peopleToLoad[[i]][1],"M",peopleToLoad[[i]][2]),collapse="")
	}
	fileName <- paste(c(fileName,".RData"),collapse="")
	
	
	if ( file.exists(fileName) && !make_new ) {
		print("File exist")
		load(fileName)
	} else {
		# test for good input and find the person index
		testPerson <- 0
		people <- peopleToLoad
		noPeople <- length(people)
		
		for(person in 1:noPeople){
			if(people[[person]][1] == group && people[[person]][2] == member ){
				testPerson <- person
			}
		}
		
		if(testPerson == 0){
			e <- simpleError(paste(c("Bad input. G",group, "M", member, " does not exist."),collapse=""))
			stop(e)
		}
		else{
			print("Good group and member input given.")
		}
		if(trainPartSize > 400 || testSize > 400){
			f <- simpleError(paste(c("Bad input. ",trainPartSize, " or ", testSize, " > 400."),collapse=""))
			stop(f)
		}
		
		# load one into the test set and all the others into the training set
		# load the data
		print("Loading data...")
		dataResult = loadAllPeople(DPI, filter,peopleToLoad, sigma=sigma, size=size)
		print("Data loaded.")
		data <- dataResult$data
		maxCipher <- dataResult$cipherSize
		
		# define different parameters
		ciphersPerChar <- 400
		
		# number of elements in total
		noTotalTrain <- trainPartSize * 10 * (noPeople - 1) # 10 different chars
		noTotalTest <- testSize * 10 
			
		train <- matrix( 0 , nrow=noTotalTrain , ncol=maxCipher) 
		test <- matrix( 0 , nrow=noTotalTest , ncol=maxCipher) 
		train_actual <- 1:noTotalTrain
		test_actual <- 1:noTotalTest
		
		
		# prepare the test and training set
		print("Preparing training and test set...")
		for(person in 1:noPeople){
			for(cipher in 1:10){
				sizeOfCipher <- dim(data[[person]][[cipher]])[2]
				if(testPerson == person){ # if it is the test guy, put in test set
					#test
					test[(((cipher - 1) * testSize) + 1):(cipher * testSize),1:sizeOfCipher] <- data[[person]][[cipher]][1:testSize,]
					test_actual[(((cipher - 1) * testSize) + 1):(cipher * testSize)] <- rep.int(cipher,testSize)
				}
				else if(testPerson > person){
					# train
					train[(((person - 1) * trainPartSize * 10) + ((cipher - 1) * trainPartSize) + 1):((person - 1) * trainPartSize * 10 + cipher * trainPartSize),1:sizeOfCipher] <- data[[person]][[cipher]][1:trainPartSize,]
					train_actual[(((person - 1) * trainPartSize * 10) + ((cipher - 1) * trainPartSize) + 1):((person - 1) * trainPartSize * 10 + cipher * trainPartSize)] <- rep.int(cipher,trainPartSize)
				}
				else{
					# train
					train[(((person - 2) * trainPartSize * 10) + ((cipher - 1) * trainPartSize) + 1):((person - 2) * trainPartSize * 10 + cipher * trainPartSize),1:sizeOfCipher] <- data[[person]][[cipher]][1:trainPartSize,]
					train_actual[(((person - 2) * trainPartSize * 10) + ((cipher - 1) * trainPartSize) + 1):((person - 2) * trainPartSize * 10 + cipher * trainPartSize)] <- rep.int(cipher,trainPartSize)
				}
			}
		}
		print("Preparation of training and test set done.Saving data...")
		
		# save the data
		finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
		
		
		save(finalData, file = fileName)
		print("Data saved.")
	
	}
	return(finalData) # return to test on it
}


prepareAllMixedCrossVal <- function(split = 0.9, crossValRuns = 10, DPI = 100 , filter = "none", peopleToLoad = getPeople(), sigma =0.5, size =5){
	
	if(split > 1){
		e <- simpleError("Bad input. Too high split (> 1).")
		stop(e)
	}
	
	# load the data
	print("Loading data...")
	dataResult = loadAllPeople(DPI, filter, peopleToLoad, sigma=sigma, size=size)
	print("Data loaded.")
	data <- dataResult$data
	maxCipher <- dataResult$cipherSize
	noPeople <- length(data)
	
	# define different parameters
	ciphersPerChar <- 400
	
	# number of elements taken from one person for the train and test sets
	trainPart <- 400*split
	testPart <- 400 - trainPart
	
	noPersonTrain <- trainPart * 10
	noPersonTest <- testPart * 10
	
	# number of elements in total
	noTotalTrain <- noPersonTrain * noPeople
	noTotalTest <- noPersonTest * noPeople
	
	train <- matrix( 0 , nrow=noTotalTrain , ncol=maxCipher) 
	test <- matrix( 0 , nrow=noTotalTest , ncol=maxCipher) 
	train_actual <- 1:noTotalTrain
	test_actual <- 1:noTotalTest
	
	# vector frames for data seperation
	train_frame <- c(rep.int(TRUE,trainPart),rep.int(FALSE,testPart))
	test_frame <- c(rep.int(FALSE,trainPart),rep.int(TRUE,testPart))
	
	# make the different sets
	startTime <- proc.time() # used for timing
	print("Preparing training and test sets...")
	for(run in 1:crossValRuns){
		for(person in 1:noPeople){
			for(cipher in 1:10){
				sizeOfCipher <- dim(data[[person]][[cipher]])[2]
				# train
				train[(((person - 1) * noPersonTrain) + ((cipher - 1) * trainPart) + 1):((person - 1) * noPersonTrain + cipher * trainPart),1:sizeOfCipher] <- data[[person]][[cipher]][train_frame,]
				train_actual[(((person - 1) * noPersonTrain) + ((cipher - 1) * trainPart) + 1):((person - 1) * noPersonTrain + cipher * trainPart)] <- rep.int(cipher,trainPart)
				#test
				test[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart),1:sizeOfCipher] <- data[[person]][[cipher]][test_frame,]
				test_actual[(((person - 1) * noPersonTest) + ((cipher - 1) * testPart) + 1):((person - 1) * noPersonTest + cipher * testPart)] <- rep.int(cipher,testPart)
			}
		}
		
		# shift the two frames
		shiftAmount <- 400/crossValRuns
		tempFrame <- c(train_frame[(401-shiftAmount):400],train_frame[1:(400-shiftAmount)])
		train_frame[] <- tempFrame[]
		tempFrame <- c(test_frame[(401-shiftAmount):400],test_frame[1:(400-shiftAmount)])
		test_frame[] <- tempFrame[]
		
		# save the data
		finalData <- list(trainSet=train,testSet=test,trainVali=train_actual,testVali=test_actual)
		fileName <- paste(c("crossVal_DPI",DPI,"_",split,"_FILTER",filter,"_",run,".RData"),collapse="")
		save(finalData, file = fileName)
		
		# timer :D
		if(run != crossValRuns){
			timer <- (((proc.time() - startTime)[3])*(crossValRuns-run)/run)
			print(paste(c("Estimated finish time within: ",timer, " seconds."),collapse=""))
		}
		
	}
	
	print("Preparation and saving of training and test set done.")
	# prepare the test and training set

	
	return(finalData) # return to test on it
}

# -- test run knn --
# currentTime <- proc.time()
# haha <- prepareAllMixed(360,40) # default: 100 dpi and no filter
# print(paste(c("Time taken to load: ",(proc.time() - currentTime)[1]),collapse=""))

# currentTime <- proc.time()
# haha <- prepareOneAlone(3,2) # default: 100 dpi and no filter
# print(paste(c("Time taken to load: ",(proc.time() - currentTime)[1]),collapse=""))

# currentTime <- proc.time()
# haha <- prepareAllMixedCrossVal() # default: 100 dpi and no filter
# print(paste(c("Time taken to load: ",(proc.time() - currentTime)[1]),collapse=""))
# 
# 
# # calc KNN
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
