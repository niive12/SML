percentageDetected   <- function(testData){
	testSets = dim(testData)[1];
	testChars = dim(testData)[2];
	testElements = dim(testData)[3];
	
	percentageVec = array(,testSets);

	for(testSet in 1:testSets){
		
		trueDetections = 0;

		# do for each testset
		for(char in 1:testChars){
			for(element in 1:testElements){
				result = KNN(tested[testSet,char,element,],trained[testSet,,,],1) #number of neighbours
				if(result == char){
					trueDetections = (trueDetections + 1)
				}
			}
		}
		percentageVec[[testSet]] = trueDetections/(testChars*testElements)
	}
	return(percentageVec);
}

person_dependent_test <- function() {
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
			print("hello")
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
			#run program
			DPI = c(100,200,300)

			# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
			RawTrainData1 = loadSinglePersonsData(DPI[[1]],g,m);
			
			# print("data aquired.")

			# split data
			# print("Spliting dataset...")
			splitList = getSystematicSplit(RawTrainData1,s,10)
			trained <- splitList$t1
			tested <- splitList$t2
			# print("Dataset split.")

			timing = proc.time()
			
			#run program
			new_data = percentageDetected(tested);
			
			timing = proc.time() - timing;
			timing = timing[["elapsed"]]
			new_data = append(timing,new_data,after=length(timing))
			
			#write down data
			data_file = read.csv(file=paste(c("result_G",g,"M",m,"_",DPI[d],"_",s,st,".csv"),collapse=""))
			name = paste(c("K",k,"_S",s,"_D",DPI[d]),collapse="")
			data_file$name = new_data;
			write.csv(data_g3m2, file=paste(c("result_G",g,"M",m,"_",DPI[d],"_",s,st,".csv"),collapse=""),row.names=FALSE)
			
		} else {
			print("bad input")
		}
	} else {
		print("The input format is: 'DPI','Split','Group number','member number','K'")
	}
}

person_dependent_test();