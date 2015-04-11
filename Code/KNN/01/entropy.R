# to do: take a vector (one pca component) and return the division point with least entropy and return the vector with all the points it was calculated for

# sum_{i=1}^{2}( S_i / S * sum_{j=0}^{9}(P(s = j)*log_2(P(s=j))) )

entropy_old <- function(component, classifications, divisions = 200, classes = 10){
	
	if(length(component) != length(classifications)){
		e <- simpleError(paste(c("Bad input. Components and classification not of same length."),collapse=""))
		stop(e)
	}
	
	max_v <- max(component)
	min_v <- min(component)
	
	interval <- (max_v - min_v)/(divisions + 1)
	
	result <- 1:divisions
	
	class_of_component <- factor(classifications,labels = 1:classes)
	
	for(div in 1:divisions){
		probabilities <- matrix(0,2,classes)
		for(i in 1:length(component)){
			if(component[i] <= (min_v + div*interval)){
				probabilities[1,class_of_component[i]] = probabilities[1,class_of_component[i]] + 1
			} else {
				probabilities[2,class_of_component[i]] = probabilities[2,class_of_component[i]] + 1
			}
		}
		S <- c(0,0)
		for(j in 1:classes){
			p_1 = 0
			p_2 = 0
				
			if(sum(probabilities[1,]) != 0){
			p_1 = probabilities[1,j]/sum(probabilities[1,])
			}
			if(sum(probabilities[2,]) != 0){
			p_2 = probabilities[2,j]/sum(probabilities[2,])
			}
			if(p_1 != 0.0){
				S[1] = S[1] - p_1 * log2(p_1)
			}
			if(p_2 != 0.0){
				S[2] = S[2] - p_2 * log2(p_2)
			}
		}
		result[div] = (sum(probabilities[1,])/ length(component))*S[1] + (sum(probabilities[2,])/ length(component))*S[2]
	}
	
	return(list(entropyList = result, divider = ((which.min(result))*interval + min_v), entropy = min(result)))
	
}

entropy <- function(component, classifications, divisions = 200, classes = 10){
	
	if(length(component) != length(classifications)){
		e <- simpleError(paste(c("Bad input. Components and classification not of same length."),collapse=""))
		stop(e)
	}
	
	max_v <- max(component)
	min_v <- min(component)
	
	total = length(component)
	
	interval <- (max_v - min_v)/(divisions + 1)
	
	result <- 1:divisions
	
	class_of_component <- factor(classifications,labels = 1:classes)
	
	probabilities_less <- matrix(0,divisions,classes)
	probabilities_total <- matrix(0,1,classes)
	
	for(i in 1:total){
		# count no. of presences
		probabilities_total[1,class_of_component[i]] = probabilities_total[1,class_of_component[i]] + 1
		# add to less side
		divs_included = ceiling((component[i] - min_v)/interval)
		if(divs_included <= divisions){
			for(j in divs_included:divisions){
				probabilities_less[j, class_of_component[i]] = probabilities_less[j, class_of_component[i]] + 1
			}
		}
		
	}
	
	for(i in 1:divisions){
		sum_less = sum(probabilities_less[i,])
		sum_more = total - sum_less
		S <- c(0,0)
		for(j in 1:classes){
			if(sum_less != 0){
				p_1 = probabilities_less[i,j]/sum_less
			}
			if(sum_more != 0){
				p_2 = (probabilities_total[1,j] - probabilities_less[i,j])/sum_more
			}
			if(p_1 != 0.0){
				S[1] = S[1] - p_1 * log2(p_1)
			}
			if(p_2 != 0.0){
				S[2] = S[2] - p_2 * log2(p_2)
			}
		}
		result[i] = (sum_less/total)*S[1] + (sum_more/total)*S[2]
	}
	
	
	return(list(entropyList = result, divider = ((which.min(result))*interval + min_v), entropy = min(result)))
	
}


# similarity test
#print(entropy(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))
#print(entropy_new(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))

# speed test
# source("load_people_data.R")
# source("pca_test.R")
# source("normalize.R")
# 
# datasetsize = 400
# 
# data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)
# # normalize data
# print("normalize")
# data <- normalizeData(data, "z-score")
# # pca
# print("pca")
# data <- pca_simplification(data, noPC = 50)
# 
# #calc entropy
# print("entropy")
# 
# startTimer <- proc.time()
# print(entropy_old(data$trainSet[,1], data$trainVali,)$entropy)
# old_time = (proc.time() - startTimer)[1]
# print(paste(c("Time taken for old: ", old_time, " seconds."), collapse = "") )
# 
# startTimer <- proc.time()
# print(entropy(data$trainSet[,1], data$trainVali)$entropy)
# new_time = (proc.time() - startTimer)[1]
# print(paste(c("Time taken for new: ", new_time, " seconds."), collapse = "") )
# 
# print(paste(c("The new method was ", (new_time/old_time)-1,"% faster."), collapse = "") )
# 