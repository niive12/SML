source("part01.R")
get_size <- function(dpi, groupNr, groupMemberNr){
	corners <- read.csv(paste(c("../../group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  
	corners = trunc(corners*dpi/300)
	
	#extract individual ciffers
	xStep = (corners[1,7]-corners[1,1])/20;
	yStep = (corners[1,8]-corners[1,2])/20;
	size  = (trunc(xStep)-2)*(trunc(yStep)-2)
	return(size)
}

KNN_test <- function(d, test_people, train_people, s, k){
	DPI = c(100,200,300)
	
	# trainingDigit (RawTrainData) is filled with the ciphers [[digit eg '0','1',...]][pixel || row, column] one row = one string of the pixel being the letter
	size = get_size(DPI[[d]],test_people[1],test_people[2]);
	RawTrainData = array(,c(10,((length(test_people)/2)*400),size))
	for(i in 1:(length(test_people)/2)){
		print(c("loading group ", test_people[i*2-1],"member ", test_people[i*2]))
		rawdata = loadSinglePersonsData(DPI[[d]],test_people[i*2-1],test_people[i*2]);
		RawTrainData[,(1+(400*(i-1))):(400*i),] = rawdata
	}
	
	# print("data aquired.")
	splitList = getSystematicSplit(RawTrainData,s,1)
	trained <- splitList$t1
	tested <- splitList$t2
	# print("Spliting dataset...")
	
	

	# split data
	
	#run program
	new_data = percentageDetected(tested,trained,k);
}

d = 1    # dpi
s = 0.5 # split tEst

p0 = c(1, 1)
# p1 = c(1, 2)
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
# p16 = c(7, 2)
p17 = c(7, 3)
all = c(p0, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p17)
# all = c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
test_people  = c(p6, p7)
train_people = p7
# k = 1:40

KNN_test(d, p6, p7, s, 10)
