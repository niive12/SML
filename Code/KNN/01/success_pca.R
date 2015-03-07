# 16m26.039s runtime
# # (2.3.1) # Nikolaj
# # all-one-vs-all and all-mixed (100 DPI)
# # Success rate + var of confusion

source("load_people_data.R")
source("pca_test.R")

acc = c(0.80,0.90,0.95,0.99)

K = 10;

fileName <- "success_pca.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
# 	one_success  = array(0,length(acc))
# 	one_variance = array(0,length(acc))
# 	mix_success  = array(0,length(acc))
# 	mix_variance = array(0,length(acc))
	if( 0 ) {
		people_data = prepareOneAlone(3,2) #9 people vs 1 person             
		for(i in 1:length(acc) ){
			data = pca_simplification(people_data,acc[i])
			res = run_knn(data,K)
			one_success[i] = res$success
			one_variance[i] = res$var
		}
		save(one_success, one_variance, mix_success, mix_variance, file = fileName)
	}
	if( 1 ) {
		people_data = prepareAllMixed(360,40) #90:10
		for(i in 1:length(acc) ){
			data = pca_simplification(people_data,acc[i])
			res = run_knn(data,K)
			mix_success[i] = res$success
			mix_variance[i] = res$var
		}
		save(one_success, one_variance, mix_success, mix_variance, file = fileName)
	}
}

colors= c("black","blue")
setEPS()
postscript("../../../Report/graphics/pca_success.eps",height = 4, width = 8)
plot(acc,one_success,type="b",lty=16, xlab="PC",ylab="Success",ylim=(c(min(c(one_success,mix_success)),max(c(one_success,mix_success)))),col=colors[1])
lines(acc,mix_success,type="b",lty=15, col=colors[2])
axis(1, at=acc)
legend("bottomleft",NULL,c("all vs one","all mixed"),cex=0.8,col=colors,lty=16:15)
quiet = dev.off()