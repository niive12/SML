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
#   ciffers <- list(readPNG(paste(c("../../group3/ciphers/Ciphers",DPI,"-0.png"), collapse = "")),
#                   readPNG(paste(c("../../group3/ciphers/Ciphers",DPI,"-1.png"), collapse = "")),
#                   readPNG(paste(c("../../group3/ciphers/Ciphers",DPI,"-2.png"), collapse = "")),
#                   readPNG(paste(c("../../group3/ciphers/Ciphers",DPI,"-3.png"), collapse = "")),
#                   readPNG(paste(c("../../group3/ciphers/Ciphers",DPI,"-4.png"), collapse = "")))
#   #load the corner values
#   corners <- read.csv("../../group3/ciphers/Corners.txt")
  
  #load the scaned images
  ciffers <- list(readPNG(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  
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
