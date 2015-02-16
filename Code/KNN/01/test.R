#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")


#-------------------------------------------------------------
#Functions to be used in evaluation
#-------------------------------------------------------------

#inspiration for smoothing
smoothImage <- function(grayImg){
  #two ways of specifying kernel:
  # kernel <- matrix( 
  #           c(1, 1, 1, 
  #             1, 1, 1, 
  #             1, 1, 1), # the data elements 
  #           3,              # number of rows 
  #           3)
  # kernel <- kernel/9
  # kernel
  
  kernel <- matrix( 
    1, # the data elements 
    3,# number of rows 
    3)
  kernel <- kernel/9
  #print(kernel)
  
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)
  
  return(smoothed)
}


#-------------------------------------------------------------
#This currently loads data according to the paths in the begining.
#Should be modified to load group members data.
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr){
#   #load the scaned images
#   ciffers <- list(readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Ciphers",DPI,"-0.png"), collapse = "")),
#                   readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Ciphers",DPI,"-1.png"), collapse = "")),
#                   readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Ciphers",DPI,"-2.png"), collapse = "")),
#                   readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Ciphers",DPI,"-3.png"), collapse = "")),
#                   readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Ciphers",DPI,"-4.png"), collapse = "")))
#   #load the corner values
#   corners <- read.csv("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers/Corners.txt")
  
  #load the scaned images
  ciffers <- list(readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  
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
    gray[[i]] <- (r+g+b)/3
  }
  
  #smooth images
  for(i in 1:5)
  {
    smoothed[[i]] <- smoothImage(gray[[i]])
  }
  
  #generate image that is prepared for learning and visualization
  for(i in 1:5)
  {
    prepared[[i]] <- smoothed[[i]]
  }
  
  
  #extract individual ciffers
  xStep <- (corners[1,7]-corners[1,1])/20;
  yStep <- (corners[1,8]-corners[1,2])/20;
  xStepT <- trunc(xStep)
  yStepT <- trunc(yStep)
  
  tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
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
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      # trainingDigit is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
      trainingDigit[[(pages-1)*2 + box]] <- tempM
    }
  }
  
  #color grid to show whats used for training
#   for(pages in 1:5)
#   {
#     for(box in 1:2)
#     {
#       for(cifX in 1:21)
#       {
#         aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
#         xStart <- aXbase-1
#         xEnd <- aXbase+1
#         for(px in xStart:xEnd)
#         {
#           for(py in corners[(pages-1)*2 + box,2]:corners[(pages-1)*2 + box,8])
#           {
#             prepared[[pages]][py,px] <- 0.0
#           }
#         }
#         
#         aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifX-1)
#         yStart <- aYbase-1
#         yEnd <- aYbase+1
#         for(py in yStart:yEnd)
#         {
#           for(px in corners[(pages-1)*2 + box,1]:corners[(pages-1)*2 + box,7])
#           {
#             prepared[[pages]][py,px] <- 0.0
#           }
#         }
#       }
#     }
#   }
#   
#   for(i in 1:5){
#     img <- gray[[i]]
#     img <- prepared[[i]]
#     display(img)
#   }
  
  
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




# generates a set of dataelements where: [[set]][training/testing (1/2)][number ('0','1',..)][trial, pixel]
# input data to split in format [[number ('0','1',..)][trial, pixel], split ratio = percentage of set being in training set, sets = number of different sets.
getSystematicSplit <- function(data, splitRatio, sets) 
{ 
  noTrials = dim(data[[1]])[1] # number of ciphers in the dataset gathered (400 ciphers)
  sizeOfCipher = dim(data[[1]])[2]
  
  print(c("Trials: ",noTrials))
  print(c("cipher: ",sizeOfCipher))
  
  forDataSet = round(noTrials*splitRatio) # number of ciphers for the training part
  forTestSet = (noTrials - forDataSet)
 
  result1 <- array(,c(sets,10,forDataSet,sizeOfCipher))
  result2 <- array(,c(sets,10,forTestSet,sizeOfCipher))
  
  print(c("train / test:", forDataSet, "/", forTestSet))
  
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

#-------------------------------------------------------------
#This is the "main function" or the code that is actualy run
#-------------------------------------------------------------
#get data from png images: 
#print(paste(c("C:/Users/Lukas Schwartz/Documents/Skole/6. semester/Statistical Machine Learning/SVNRepository/group3/ciphers",DPI,"-0.png"), collapse = ""))

print("Getting data...")

DPI = c(100,200,300)


# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
RawTrainData1 = loadSinglePersonsData(DPI[[2]],3,2);

print("data aquired.")

# split data
print("Spliting dataset...")
splitList = getSystematicSplit(RawTrainData1,0.5,10)
trained <- splitList$t1
tested <- splitList$t2
print("Dataset split.")

#display(trained[1,1,,])

# print(KNN(tested[1,6,1,],trained[1,,,],10))
# 
# img <- array(,c(20,19))
# for(l in 1:20)
# {
#   img[l,] <- tested[1,6,1,((l-1)*19+1):(l*19)]
# }
# 
# display(img)

# test dataset using KNN
testSets <- dim(tested)[1]
testChars <- dim(tested)[2]
testElements <- dim(tested)[3]

for(testSet in 1:testSets)
{
  trueDetections <- 0
  
  # do for each testset
  for(char in 1:testChars)
  {
    for(element in 1:testElements)
    {
      result <- KNN(tested[testSet,char,element,],trained[testSet,,,],1) # number of neighbours
      
      if(result == char)
      {
        trueDetections <- (trueDetections + 1)
      }
    }
  }
  
  # output result
  print(c(testSet, "Correctly detected:",(trueDetections/(testChars*testElements))))
}


