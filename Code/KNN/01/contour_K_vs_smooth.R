# 48h50m20.349s
library("grDevices") # gray.colors

source("load_people_data.R")
source("pca_test.R")

# sigma = 1.7, size = 9

makeContoursSmoothing <- function(x,y,title,xlab,ylab,file_name){
	startTime <- proc.time() # used for timing
	contour_data = matrix(,length(x),length(y))
	for(x_val in 1:length(x)){
		data = prepareOneAlone(group=3,member=2, trainPartSize=400, testSize=400, peopleToLoad=getPeople(), filter="gaussian", make_new=1,size=9,sigma=x[x_val])
		for(y_val in 1:length(y)){
			contour_data[x_val,y_val] <- ((run_knn(data,K=y[y_val]))$success)
			# timer :D
			timer <- ((proc.time() - startTime)[1])
			print(paste(c((length(y)*(x_val-1) + y_val), "/",(length(x)*length(y))," runs completed. Time used till now: ",timer, " seconds."),collapse=""))
		}
	}

	# plot the contour plot
	setEPS()
	postscript(file_name,height = 6, width = 8)
	filled.contour(y = y, x = x, contour_data, col=colorpanel(10, "white", "black"), levels=seq(min(contour_data), max(contour_data), length.out= 11))
	title(main = title, xlab = xlab, ylab = ylab)
	q = dev.off()
}


# PCA on 1 person only, all-one-vs-all and all-mixed (100 DPI)
# Contour K vs Acc. PC var.
# K: 1:1:20
# PCA: 50:5:100%

# actual measured points in ascending order
k = 1:20
sigma =  c(seq(0.1,2,0.1))

makeContoursSmoothing(x=sigma,y=k,title="Success Of Smoothing",xlab="Sigma",ylab="K",file_name="../../../Report/graphics/contour_k_smooth_oneVsRest.eps")