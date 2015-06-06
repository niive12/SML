# run full test on the best dataset, easy and hard problem
library("gplots") # for colorpanel

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("confusion_matrix.R")

trainSetSize = 400
testSetSize = 400

s_k = 1
s_size = 5
s_sigma = 0.9
s_pc = 40
s_crossrefs = 10

people = getPeople()
noPeople = length(people)
success_hard = 1:noPeople
success_easy = 1:s_crossrefs

confus_easy = matrix(0,10,10)
confus_hard = matrix(0,10,10)

if(F){
	# data easy problem
	data_easy = prepareAllMixedCrossVal(split = 0.9, crossValRuns = s_crossrefs, filter = "gaussian", size = s_size, sigma = s_sigma, make_new = 1, peopleToLoad = people)
	for(i in 1:s_crossrefs){
		data_easy_s = normalizeData(data_easy[[i]], normMethod = "z-score")
		data_easy_s = pca_simplification(data_easy_s, noPC = s_pc)
		data_easy_s = normalizeData(data_easy_s, normMethod = "z-score")
		
		knn_easy = run_knn(data_easy_s, s_k)
		success_easy[i] = knn_easy$success
		confus_easy = confus_easy + knn_easy$confus
	}
	
	print("Easy problem done.")
	
	# data hard problem
	for(person in 1:noPeople){
		data_hard = prepareOneAlone(people[[person]][1], people[[person]][2],  trainPartSize = trainSetSize, testSize = testSetSize, make_new = 1, filter = "gaussian", size = s_size, sigma = s_sigma, peopleToLoad = people)
		data_hard = normalizeData(data_hard, normMethod = "z-score")
		data_hard = pca_simplification(data_hard, noPC = s_pc)
		data_hard = normalizeData(data_hard, normMethod = "z-score")
		
		knn_hard = run_knn(data_hard, s_k)
		success_hard[person] = knn_hard$success
		confus_hard = confus_hard + knn_hard$confus
	}
	
	#save adata
	save(success_hard, success_easy, confus_hard, confus_easy, file = "KNN_final_full_test.RData")	
	
} else{
	# load old data
	load("KNN_final_full_test.RData")
}


# plot hard
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

setEPS()
postscript("../../../Report/graphics/knn_final_full_hard.eps",height = 6, width = 8)
plot(1:noPeople,success_hard, xaxt="n",type="b",xlab="Person",ylab="Success Rate") 
abline(h=mean(success_hard), col = "red")
axis(1, at=1:noPeople, labels=x_lab, las = 2)
dev.off()

#plot easy
setEPS()
postscript("../../../Report/graphics/knn_final_full_easy.eps",height = 4, width = 8)
par(mar=c(1, 4, 4, 1) + 0.1)
boxplot(success_easy,ylab="Success Rate", outline = F) 
dev.off()

# plot confusions
confusion_matrix(confus_easy, filename="../../../Report/graphics/knn_confusion_bestparam_easy.eps")
confusion_matrix(confus_hard, filename="../../../Report/graphics/knn_confusion_bestparam_hard.eps")

print(paste(c("Mean / Var of the hard: ", mean(success_hard), " / ", var(success_hard)), collapse = ""))
print(paste(c("Mean / Var of the easy: ", mean(success_easy), " / ", var(success_easy)), collapse = ""))

