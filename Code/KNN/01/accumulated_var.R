# 3m58.757s - 0m0.705s runtime
# (2.1.1/2) # nikolaj
# PCA on 1 person only, all-one-vs-all and all-mixed (100 DPI)
# variance + accumulated var for 20 first PC
source("load_people_data.R")
source("pca_test.R")

x = 1:20

variance    = data.frame(one_person = x)
accumulated = data.frame(one_person = x)


fileName <- "accumulated_variance_data.RData" #to save time, runtime without this is 0m48.832s
if ( file.exists(fileName) && 1 ) { #toggle wether we want new data or not
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
	data = pca_simplification(data)
	variance$one_person = data$variance[x]/sum(data$variance)
	accumulated$one_person = cumsum(variance$one_person)

	data = prepareOneAlone(3,1) #9 people vs 1 person
	data = pca_simplification(data)
	variance$all_one = data$variance[x]/sum(data$variance)
	accumulated$all_one = cumsum(variance$all_one)

	data = prepareAllMixed(360,40) #90:10
	data = pca_simplification(data)
	variance$all_mix = data$variance[x]/sum(data$variance)
	accumulated$all_mix = cumsum(variance$all_mix)
	save(variance, accumulated, file = fileName)
}

colors= c("black","blue","purple")
setEPS()
postscript("../../../Report/graphics/pca_variance.eps",height = 4, width = 8)

for(d in 1:3){
	if(d == 1) {
		plot(x,variance[[d]],type="b",xlab="PC",ylab="variance",ylim=(c(0,max(variance))),col=colors[d])
	} else {
		lines(x,variance[[d]],type="b",lty=d, col=colors[d])
	}
}	
legend("topright",NULL,c("one_person","all vs one","all mixed"),cex=0.8,col=colors,lty=1:3)
quiet = dev.off()

setEPS()
postscript("../../../Report/graphics/pca_acc_variance.eps",height = 4, width = 8)

for(d in 1:3){
	if(d == 1) {
		plot(x,accumulated[[d]],type="b",xlab="PC",ylab="variance",ylim=(c(0,max(accumulated))),col=colors[d])
	} else {
		lines(x,accumulated[[d]],type="b",lty=d, col=colors[d])
	}
}	
legend("bottomright",NULL,c("one_person","all vs one","all mixed"),cex=0.8,col=colors,lty=1:3)
quiet = dev.off()