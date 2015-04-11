# plot the information gain for the PC's and show which PC it is on plot

library("graphics")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("entropy.R")


divisions = 100
pc_plot = 5

datasetsize = 10


data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)
# normalize data
print("normalize")
data <- normalizeData(data, "z-score")
# pca
print("pca")
data <- pca_simplification(data, noPC = pc_plot)

#calc entropy
print("entropy")
entropy_data <- matrix(0,2,pc_plot) # for both PCi and value
startTimer <- proc.time()
for( i in 1:pc_plot) {
	entropy_data[1,i] = (entropy(data$trainSet[,i], data$trainVali, divisions)$entropy)
	entropy_data[2,i] = i
	print(paste(c(i , " / ", pc_plot, ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (pc_plot-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
}

# sort data
print("sort the data")
entropy_data = entropy_data[,order(entropy_data[1,])]





