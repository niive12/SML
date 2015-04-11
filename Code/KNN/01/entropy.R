# to do: take a vector (one pca component) and return the division point with least entropy and return the vector with all the points it was calculated for

# sum_{i=1}^{2}( S_i / S * sum_{j=0}^{9}(P(s = j)*log_2(P(s=j))) )

entropy <- function(component, classifications, divisions = 20, classes = 10){
	
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
			if(component[i] < (min_v + div*interval)){
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

# print(entropy(c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0), c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 5, 5))