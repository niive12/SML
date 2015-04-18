# normalize the data...
source("entropy.R")

normalize <- function(x){
	return((x-min(x))/(max(x)-min(x)))
}


bin <- function(x, bin, inverse = TRUE){
	min = min(x)
	max = max(x)
	binSize = (max-min)/bin
	
	for(feature in 1:length(x)){
		bini = 1
		while(x[feature] > (min + bini*binSize) && bini < bin){
			bini = bini +1
		}
		x[feature] = (bini-1)
		if(inverse){
			x[feature] = (bin-bini)
		}
	}
	return(x)
}


normalizeData <- function(data, normMethod = "min-max", bins = 2, inverseBinning = TRUE, ent_divs = 200){ # data must be a list with a trainSet adn testSet entry
	trainS = matrix(,dim(data$trainSet)[1],dim(data$trainSet)[2])
	testS = matrix(,dim(data$testSet)[1],dim(data$testSet)[2])
	
	if(normMethod == "min-max"){
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] = normalize(data$trainSet[i,])
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] = normalize(data$testSet[i,])
		}
	}
	else if(normMethod == "z-score"){
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] = scale(data$trainSet[i,], scale = TRUE)
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] = scale(data$testSet[i,], scale = TRUE)
		}
	}else if(normMethod == "bin"){
		if(bins < 2){
			# generate error..
			e <- simpleError(paste(c("Bad input. No. of bins too low, bins must be >= 2. You typed: ", bins, " < 2"),collapse=""))
			stop(e)
		}
		
		for(i in 1:dim(data$trainSet)[1]){
			trainS[i,] <- bin(data$trainSet[i,], bins, inverseBinning)
		}
		for(i in 1:dim(data$testSet)[1]){
			testS[i,] <- bin(data$testSet[i,], bins, inverseBinning)
		}
	}else if(normMethod == "entropy"){
		for(PC in 1:dim(data$trainSet)[2]){
			ent_div = entropy(data$trainSet[,PC], data$trainVali, divisions = ent_divs, classes = 10)$divider
			
			# train set
			for(i in 1:dim(data$trainSet)[1]){
				trainS[i,PC] = F
				if(data$trainSet[i,PC] <= ent_div){
					trainS[i,PC] = T
				}
			}
			# test set
			for(i in 1:dim(data$testSet)[1]){
				testS[i,PC] = F
				if(data$testSet[i,PC] <= ent_div){
					testS[i,PC] = T
				}
			}
		}
	}
	
	return(list(trainSet = trainS, testSet = testS,trainVali=data$trainVali,testVali=data$testVali))
}


countEntries <- function(dataRow, dataElements){
	result <- matrix(0,1,length(dataElements))
	
	for(index in 1:length(dataRow)){
		element = 1;
		while(dataRow[index] != dataElements[element] && element <= length(dataElements)){
			element = element + 1
		}
		if(element == length(dataElements) +1){
			# generate error..
			e <- simpleError(paste(c("Bad input. Dataelement '", dataRow[index],"' at position ", index," not found in given list."),collapse=""))
			stop(e)
		}
		result[1,element] = result[element] + 1
	}
	return (result[1,])
}


countBinning <-function(data, charWidth, dataElements, pictureDevisions = 1){
	charHeight <- (dim(data$trainSet)[2])/charWidth
	
	trainS <- matrix(0,dim(data$trainSet)[1],length(dataElements)*pictureDevisions)
	testS <- matrix(0,dim(data$testSet)[1],length(dataElements)*pictureDevisions)
	
	if(charHeight < pictureDevisions){
		# generate error..
		e <- simpleError(paste(c("Bad input. Too high picture devision: ",pictureDevisions, " > ", charHeight, " ( = current char height), in 'countBinning'."),collapse=""))
		stop(e)
	}
	
	levels = charHeight/pictureDevisions
	
	for(i in 1:dim(data$trainSet)[1]){
		for(devision in 1:pictureDevisions){
			trainS_s = ((devision - 1)*length(dataElements) + 1)
			trainS_e = (devision*length(dataElements))
			trainSet_s = ((round(levels*(devision-1))*charWidth) + 1)
			trainSet_e = (round(levels*devision)*charWidth)
			
			if(devision == pictureDevisions){
				trainSet_e = (dim(data$trainSet)[2])
			}
			
			trainS[i,trainS_s:trainS_e] = countEntries((data$trainSet[i,trainSet_s:trainSet_e]),dataElements)
		}
	}
	
	for(i in 1:dim(data$testSet)[1]){
		for(devision in 1:pictureDevisions){
			testS_s = ((devision - 1)*length(dataElements) + 1)
			testS_e = (devision*length(dataElements))
			testSet_s = ((round(levels*(devision-1))*charWidth) + 1)
			testSet_e = (round(levels*devision)*charWidth)

			if(devision == pictureDevisions){
				testSet_e = (dim(data$trainSet)[2])
			}
			
			testS[i,testS_s:testS_e] = countEntries(data$testSet[i,testSet_s:testSet_e],dataElements)
		}
	}
	
	return(list(trainSet = trainS, testSet = testS,trainVali=data$trainVali,testVali=data$testVali))
}


centerOfMass <- function(dataRow, charWidth){
	charHeight <- length(dataRow)/charWidth
	
	if(floor(charHeight) != charHeight ){
		# generate error..
		e <- simpleError(paste(c("Bad input. charWidth not valid. Picture must form a square."),collapse=""))
		stop(e)
	}
	
	
	completeMass = sum(dataRow)
	
	x_com <- 0
	y_com <- 0
	
	x_mass_dist <- matrix(0,1,charWidth)
	y_mass_dist <- matrix(0,1,charHeight)
	
	# x mass dist
	for(x_mass in 1:charWidth){
		x_mass_dist <- x_mass_dist + dataRow[((x_mass-1)*charWidth + 1):(x_mass*charWidth)]
	}
		
	# y mass dist
	for(y_mass in 1:charHeight){
		y_mass_dist[1,y_mass] <- sum(dataRow[((y_mass-1)*charWidth + 1):(y_mass*charWidth)])
	}
		
	# com in x
	for(x in 1:charWidth){
		x_com <- x_com + x_mass_dist[1,x]*x
	}
	x_com <- x_com / completeMass
	
	# com in y
	for(y in 1:charHeight){
		y_com <- y_com + y_mass_dist[1,y]*y
	}
	y_com <- y_com / completeMass
	
	
	return (list(x = x_com,y = y_com))
}


cropDigitLimits <- function(dataRow, charWidth, is2binned = FALSE){
	charHeight <- length(dataRow)/charWidth
	
	if(floor(charHeight) != charHeight ){
		# generate error..
		e <- simpleError(paste(c("Bad input. charWidth not valid. Picture must form a square."),collapse=""))
		stop(e)
	}
	
	if(!is2binned){
		dataRow <- bin(dataRow, 2, TRUE)
	}
	
	x_b <- 1
	x_e <- charWidth
	y_b <- 1
	y_e <- charWidth
	
	x_mass_dist <- matrix(0,1,charWidth)
	y_mass_dist <- matrix(0,1,charHeight)
	
	# x mass dist
	for(x_mass in 1:charWidth){
		x_mass_dist <- x_mass_dist + dataRow[((x_mass-1)*charWidth + 1):(x_mass*charWidth)]
	}
	
	# y mass dist
	for(y_mass in 1:charHeight){
		y_mass_dist[1,y_mass] <- sum(dataRow[((y_mass-1)*charWidth + 1):(y_mass*charWidth)])
	}
	
	# xb
	x = 1
	while(x_mass_dist[x] == 0){
		x <- x + 1
	}
	x_b <- x
	
	# xe
	x = charWidth
	while(x_mass_dist[x] == 0){
		x <- x - 1
	}
	x_e <- x
	
	# yb
	y = 1
	while(y_mass_dist[y] == 0){
		y <- y + 1
	}
	y_b <- y
	
	# ye
	y = charWidth
	while(y_mass_dist[y] == 0){
		y <- y - 1
	}
	y_e <- y
	
	
	return (list(xb = x_b, yb = y_b, xe = x_e, ye = y_e))
}


cropDigit <- function(dataRow, charWidth, cropCords){
	
	# make frame for it
	frame <- rep.int(FALSE,length(dataRow))
	# everything to remove above the picture
# 	frame[1:((cropCords$yb-1)*charWidth)] <- rep.int(FALSE,(cropCords$yb-1)*charWidth)
# 	# the pic(ture minus the sides
	frame_x <- 1:charWidth
	frame_x[1:charWidth] <- c(rep.int(FALSE,cropCords$xb-1),rep.int(TRUE,(cropCords$xe-cropCords$xb + 1)),rep.int(FALSE,(charWidth-cropCords$xe)))
	frame[((cropCords$yb - 1)*charWidth + 1):(cropCords$ye*charWidth)] <- rep(frame_x,(cropCords$ye-cropCords$yb + 1))
# 	# averything below the image
# 	frame[(cropCords$ye*charWidth + 1):(length(dataRow))] <- rep(FALSE,(length(dataRow)-(cropCords$ye*charWidth)))
	
	cropped_pic <- matrix(,1,length(dataRow[frame]))

	index = 1
	for(i in 1:length(dataRow)){
		if(frame[i] == TRUE){
			cropped_pic[index] <- dataRow[i]
			index <- index + 1
		}
	}

	return (cropped_pic)
}

# test countEntries
# testlist <- c(1,2,1,1,1,1,1,2,2,2,3,2,2,3,1,3,1,2,2,1,1) # 1 = 10, 2 = 8, 3 = 3
# print(countEntries(testlist,1:3))

# #  test of bin in normalizeData
# testarray = c(0.1,0.8,0.1,1,0,0.9,0.1,0.6,0.4,0.55)
# result <- bin(testarray,2, inverse = T)
# print(result)

# test centerOfMass
# testarray = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# print(centerOfMass(testarray,4))

# test crop
# image <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
# result <- cropDigit(image, 5, (list(xb = 2, yb = 3, xe = 4, ye = 5)))
# print(result)

# #  test of bin in normalizeData
# testarray = c(0.1,0.8,0.1,1,0,0.9,0.1,0.6,0.4,0.55)
# result <- bin(testarray,2, inverse = T)
# print(result)
