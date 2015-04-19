# to do: take a vector (one pca component) and return the division point with least entropy and return the vector with all the points it was calculated for

# sum_{i=1}^{2}( S_i / S * sum_{j=0}^{9}(P(s = j)*log_2(P(s=j))) )

entropy <- function(component, classifications, divisions = 200, classes = 10){
	return(entropy_v3(component, classifications, divisions = 200, classes = 10))
}

entropy_v1 <- function(component, classifications, divisions = 200, classes = 10){
	
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

entropy_v2 <- function(component, classifications, divisions = 200, classes = 10){
	
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

entropy_v3 <- function(component, classifications, divisions = 200, classes = 10){
	
	total = length(component)
	
	if(total != length(classifications)){
		e <- simpleError(paste(c("Bad input. Components and classification not of same length."),collapse=""))
		stop(e)
	}
	
	
	class_of_component <- factor(classifications,labels = 1:classes)
	
	accurances <- matrix(0,2,classes) # first [1,] = total, second [2,] = accurance till now
	
	order_elements <- order(component)
	
	ordered_comp = component[order_elements]
	ordered_class = class_of_component[order_elements]
	
	max_v <- ordered_comp[total]
	min_v <- ordered_comp[1]
	
	interval <- (max_v - min_v)/(divisions + 1)
	
	result <- 1:divisions
	
	for(i in 1:total){
		# count no. of presences
		accurances[1,ordered_class[i]] = accurances[1,ordered_class[i]] + 1	
	}
	
	div = 1
	for(i in 1:total){
		# if division is passed, calc entropy
		if(ordered_comp[i] > (min_v + div * interval) && div <= divisions){
			sum_less = sum(accurances[2,])
			sum_more = total - sum_less
			S <- c(0,0)
			for(j in 1:classes){
				if(sum_less != 0){
					p_1 = accurances[2,j]/sum_less
				}
				if(sum_more != 0){
					p_2 = (accurances[1,j] - accurances[2,j])/sum_more
				}
				if(p_1 != 0.0){
					S[1] = S[1] - p_1 * log2(p_1)
				}
				if(p_2 != 0.0){
					S[2] = S[2] - p_2 * log2(p_2)
				}
			}
			result[div] = (sum_less/total)*S[1] + (sum_more/total)*S[2]		
			# inc division
			div = div + 1
		}
		# add accurance
		accurances[2,ordered_class[i]] = accurances[2,ordered_class[i]] + 1
	}
	
	return(list(entropyList = result, divider = ((which.min(result))*interval + min_v), entropy = min(result)))
}

# similarity test
# print(entropy_v1(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))
# print(entropy_v2(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))
# print(entropy_v3(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))

# # speed test
# source("load_people_data.R")
# source("pca_test.R")
# 
# datasetsize = 400
# 
# print("Load")
# data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)
# 
# # pca
# print("pca")
# data <- pca_simplification(data, noPC = 50)
# 
# #calc entropy
# print("entropy")
# 
# startTimer <- proc.time()
# print(entropy_v1(data$trainSet[,1], data$trainVali,)$entropy)
# time_v1 = (proc.time() - startTimer)[1]
# print(paste(c("Time taken for v1: ", time_v1, " seconds."), collapse = "") )
# 
# startTimer <- proc.time()
# print(entropy_v2(data$trainSet[,1], data$trainVali,)$entropy)
# time_v2 = (proc.time() - startTimer)[1]
# print(paste(c("Time taken for v2: ", time_v2, " seconds."), collapse = "") )
# 
# startTimer <- proc.time()
# print(entropy_v3(data$trainSet[,1], data$trainVali,)$entropy)
# time_v3 = (proc.time() - startTimer)[1]
# print(paste(c("Time taken for v3: ", time_v3, " seconds."), collapse = "") )
# 
# print(paste(c("V1 was ", (time_v2/time_v1)-1,"% faster than V2 and ", (time_v3/time_v1)-1,"% faster than V3."), collapse = "") )
# print(paste(c("V2 was ", (time_v1/time_v2)-1,"% faster than V1 and ", (time_v3/time_v2)-1,"% faster than V3."), collapse = "") )
# print(paste(c("V3 was ", (time_v1/time_v3)-1,"% faster than V1 and ", (time_v2/time_v3)-1,"% faster than V2."), collapse = "") )
