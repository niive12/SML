# 17m21.895s runtime
# library("gplots") # needs to be installe

library("grDevices") # gray.colors

source("load_people_data.R")
source("pca_test.R")

# sigma = 1.7, size = 9

makeContoursSmoothing <- function(x,y,title,xlab,ylab,file_name){
	startTime <- proc.time() # used for timing
	contour_data = matrix(,length(x),length(y))
	for(x_val in 1:length(x)){
		for(y_val in 1:length(y)){
			data = prepareOneAlone(group=3,member=2, trainPartSize=400, testSize=400, peopleToLoad=getPeople(), filter="gaussian", make_new=1,size=9,sigma=x[x_val])
			contour_data[x_val,y_val] <- ((run_knn(data,K=10))$success)
			# timer :D
			timer <- ((proc.time() - startTime)[1])
			print(paste(c((length(y)*(x_val-1) + y_val), "/",(length(x)*length(y))," runs completed. Time used till now: ",timer, " seconds."),collapse=""))
		}
	}
	
	# plot the contour plot
# 	filled.contour(y = y, x = x, contour_data, color.palette = heat.colors)
	setEPS()
	postscript(file_name,height = 6, width = 8)
	filled.contour(y = y, x = x, contour_data, color.palette = gray.colors)
	title(main = title, xlab = xlab, ylab = ylab)
	q = dev.off()
	# filled.contour(y = y, x = trainingSetSize, contour_data, col=colorpanel(20, "grey10", "white"), nlevels=20)
	# 
}


# PCA on 1 person only, all-one-vs-all and all-mixed (100 DPI)
# Contour K vs Acc. PC var.
# K: 1:1:20
# PCA: 50:5:100%

testPerson <- c(3,2)


# actual measured points in ascending order
PCA = c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
k = 1:20
size =  c(seq(3,15,2))
sigma = c(seq(0.1,2,0.1))

makeContoursSmoothing(x=sigma,y=size,title="Success Of Smoothing",xlab="Sigma",ylab="Kernel Size",file_name="../../../Report/graphics/contour_k_smooth_oneVsRest.eps")
data <- prepareOneAlone(group=3,member=2, trainPartSize=400, testSize=400, peopleToLoad=getPeople(), filter="gaussian", make_new=1,sigma=1231, size=9)
makeContours(PCA,k,data,"contour_k_smooth_oneVsRest.eps")
