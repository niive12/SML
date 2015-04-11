# plot the information gain for the PC's and show which PC it is on plot

library("graphics")

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("entropy.R")


divisions = 100
pc_calc = 5
pc_plot = 5

datasetsize = 10


data = prepareOneAlone(3,2,datasetsize, datasetsize, 100)
# normalize data
print("normalize")
data <- normalizeData(data, "z-score")
# pca
print("pca")
data <- pca_simplification(data, noPC = pc_calc)

#calc entropy
print("entropy")
entropy_data <- matrix(0,2,pc_calc) # for both PCi and value
startTimer <- proc.time()
for( i in 1:pc_calc) {
	entropy_data[1,i] = (entropy(data$trainSet[,i], data$trainVali, divisions)$entropy)
	entropy_data[2,i] = i
	print(paste(c(i , " / ", pc_calc, ". Time taken till now: ", ((proc.time() - startTimer)[3]), " seconds. Estimated finish in: ", (pc_calc-i)*((proc.time() - startTimer)[3])/i), collapse = "") )
}

# sort data
print("sort the data")
entropy_data = entropy_data[,order(entropy_data[1,])]

# make x_lab list
x_lab = 1:pc_calc
for(i in 1:pc_calc){
	x_lab[i] = paste(c("PC",entropy_data[2,i]), collapse = "")
}

# make IG
IG = 1:pc_plot
IG[1] = 0
for(i in 2:pc_calc){
	IG[i] = entropy_data[1,i-1] - entropy_data[1,i]
}


# plot
setEPS()
postscript("entropy_pc.eps",height = 6, width = 8)
plot(1:pc_plot,IG, xlab="PC", ylab="Information Gain",type="l",lty=1)
axis(1, at=1:pc_plot, labels=x_lab)
q = dev.off()

plot(1:pc_plot,IG, xlab="PC", ylab="Information Gain",type="l",lty=1)
axis(1, at=1:pc_plot, labels=x_lab)
