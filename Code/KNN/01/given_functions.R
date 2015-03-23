#Functions to be used in evaluation
#-------------------------------------------------------------

library("png")
library("EBImage")
library("class")
library("gmodels")

#inspiration for smoothing
smoothImage <- function(grayImg){
	#manual kernel:
	kernel <- matrix(c(0, 1, 0, 
	                   1, 1, 1, 
	                   0, 1, 0), # the data elements 
	                   3,3)
	kernel <- kernel/5
	
	#using r library for smoothing
	smoothed <- filter(grayImg, kernel, method="convolution", sides=2)
	
	return(smoothed)
}
gaussianSmoothImage <- function(grayImg, sigma, size){
	kernel <- matrix(1,     # the data elements 
					 size,  # number of rows 
					 size)  # number of columns
	os = (size-1)/2
	for(x in (-os):os ){
		for(y in (-os):os ){
			res = 340*(1/(2*pi*sigma^2)) * exp(-1 * (x^2+y^2)/(2*sigma^2))
			kernel[[x+os+1,y+os+1]] = (1/(2*pi*sigma^2))*exp(-1*(x^2+y^2)/(2*sigma^2))
		}
	}
	kernel = kernel * 1/sum(kernel)
	print("hello")
	smoothed <- filter(grayImg, kernel, method="convolution", sides=2)
	return(smoothed)
}


#-------------------------------------------------------------
#This currently loads data according to the paths in the begining.
#Should be modified to load group members data.
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr, smooth="none", sigma=1, size = 5, make_new=0){
	#   #load the scaned images
	#load the scaned images
	fileName <- paste(c("G",groupNr,"M",groupMemberNr,"_DPI",DPI,".RData"),collapse="")
	
	# working director with the data
	# must be two folders into the folder with the data (in a member folder) because the loadSinglePersonsData function from given_functions currently defines it..
	
	if ( file.exists("personalpath.RData") ) {
		dataDepository = "/home/niko/dokumenter/machine_learning/trunk" # nikolaj
	} else {
		dataDepository = "C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository" # lukas	
	}
	
	
	if(file.exists(fileName) && !make_new){
		print("loading old loadSinglePersonsData")
		load(fileName)
	} else{
		# change the working directory to where the data is
		currentWD <- getwd()
		setwd(dataDepository)
		
		ciffers <- list(readPNG(paste(c("group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
						readPNG(paste(c("group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
						readPNG(paste(c("group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
						readPNG(paste(c("group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
						readPNG(paste(c("group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
		#load the corner values
		corners <- read.csv(paste(c("group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
		
		setwd(currentWD)
		
		corners <- trunc(corners*DPI/300)
		#print(corners)
		
		#define lists to be used
		gray <- list(1:5)
		smoothed <- list(1:5)
		prepared <- list(1:5)
		
		
		#convert the images to gray scale.
		for(i in 1:5)
		{
			r <-ciffers[[i]][,,1]
			g <-ciffers[[i]][,,2]
			b <-ciffers[[i]][,,3]
			gray[[i]] <- (r*0.3+g*0.6+b*0.1)
		}
		
		#   #smooth images
		if(smooth == "average" ){
			for(i in 1:5) {
				smoothed[[i]] <- smoothImage(gray[[i]])
			}            
		} else if(smooth == "gaussian" ) {
			for(i in 1:5) {
				smoothed[[i]] <- gaussianSmoothImage(gray[[i]],sigma, size)
			}
		} else {
			smoothed = gray;
		}
		#   
		#generate image that is prepared for learning and visualization
		for(i in 1:5)
		{
			prepared[[i]] <- smoothed[[i]]
		}
		
		
		#extract individual ciffers
		xStep <- (corners[1,7]-corners[1,1])/20;
		yStep <- (corners[1,8]-corners[1,2])/20;
		
		# 		xStepT <- trunc(xStep)
		xStepT <- 60*DPI/300
		# 		yStepT <- trunc(yStep)
		yStepT <- 60*DPI/300
		
		# 		tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
		# 		trainingDigit <- list(1:10);
		# 		
		# 		for(pages in 1:5)
		# 		{
		# 			for(box in 1:2)
		# 			{
		# 				#     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
		# 				for(cifX in 1:20)
		# 				{
		# 					aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
		# 					for(cifY in 1:20)
		# 					{
		# 						aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
		# 						
		# 						for(px in 2:xStepT-1)
		# 						{
		# 							for(py in 2:yStepT-1)
		# 							{
		# 								tempM[(cifY-1)*20 + cifX, (px-2)*(yStepT-2) + py-1] <- prepared[[pages]][aYbase+py,aXbase+px]
		# 							}
		# 						}
		# 					}
		# 				}
		# 				# trainingDigit is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
		# 				trainingDigit[[(pages-1)*2 + box]] <- tempM
		# 			}
		# 		}
		# 		save(trainingDigit, file = fileName)
		
		tempM <- matrix(,20*20,(yStepT)*(xStepT))
		trainingDigit <- list(1:10);
		
		for(pages in 1:5)
		{
			for(box in 1:2)
			{
				#     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
				for(cifX in 1:20)
				{
					aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
					for(cifY in 1:20)
					{
						aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
						
						for(px in 1:xStepT)
						{
							for(py in 1:yStepT)
							{
								tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
							}
						}
					}
				}
				# trainingDigit is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
				trainingDigit[[(pages-1)*2 + box]] <- tempM
			}
		}
		save(trainingDigit, file = fileName)
	}
	
	
	return(trainingDigit)
}



getRandomSplit <- function(trainingData, trainingPc){
	#randomly split data in a balanced maner:
	trainL = list(1:length(trainingData))
	testL = list(1:length(trainingData))
	for(Nr in 0:(length(trainingData)-1))
	{
		amountEachNumber = nrow(trainingData[[Nr+1]]);
		
		set.seed(1) ## make randomness reproducible here
		rand <- sample(amountEachNumber) #generate  a randomly ordered indexing list the size of the datasample
		
		vector = c(1:amountEachNumber);
		for(i in 1:trunc(amountEachNumber*trainingPc))
		{
			vector[i] = 1;
		}
		for(i in trunc(amountEachNumber*trainingPc)+1:amountEachNumber)
		{
			vector[i] = 2;
		}
		splittingIndexer = vector[rand]
		splitData <- split.data.frame(trainingData[[Nr+1]], factor(splittingIndexer))
		
		trainL[[Nr+1]] <- splitData[[1]]
		testL[[Nr+1]]<- splitData[[2]]
	}  
	
	training <- trainL[[1]]
	testing <- testL[[1]]
	trainClass <- rep(0,nrow(trainL[[1]]) )
	testClass <- rep(0,nrow(testL[[1]]) )
	for(i in 2:10)
	{
		training <- rbind(training, trainL[[i]])
		testing <- rbind(testing, testL[[i]])
		
		trainClass <- append(trainClass, rep(i-1,nrow(trainL[[i]]) ) )
		testClass <- append(testClass, rep(i-1,nrow(testL[[i]]) ) )
	}
	trainClassF <- factor(trainClass)
	testClassF <- factor(testClass)
	
	return(list(training, testing, trainClassF, testClassF))
	
}
