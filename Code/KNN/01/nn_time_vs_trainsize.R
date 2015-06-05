source("load_people_data.R")
source("normalize.R")
source("neural_network_simplified.R")
source("pca_test.R")
source("dual_plot.R")

fileName <- "nn_time_vs_train.RData"

nn_time_vs_train <- function(data, train_size, plot_name){
	print(train_size)
	startTime = proc.time() # used for timings
	
	normalize_setting = list(normMethod = "z-score")
	pca_setting         = list(noPC=100)
	
	time_model   = array(0,length(train_size))
	time_predict = array(0,length(train_size))
	result       = array(0,length(train_size))
	
	n_hidden_layers = 200
	
	for(i in 1:length(train_size)){
		
		load_setting = list(trainPartSize = train_size[i], testSize = 10)
		data <- prepareOneAloneNormPCA(3,2,load_setting,normalize_setting,pca_setting)
		# run mlp
		tmp_time_model = proc.time()
		model = neural_network_simplification(data,size=n_hidden_layers)
		time_model[i] = (proc.time()-tmp_time_model)[["user.self"]]
		
		tmp_time_predict = proc.time()
# 		result[i] <- 1
		result[i] <- neural_network_predict(model,data)$success
		time_predict[i] = (proc.time()-tmp_time_predict)[["user.self"]]
		
		print(c(time_model[i],result[i],time_predict[i]))
		
		print(paste(c( i , "/", length(train_size), " datasets loaded. Time taken till now: ",(proc.time()-startTime)[["user.self"]], " seconds."),collapse = ""))
		save(result, time_model, time_predict, file = fileName)
		dual_plot(x=train_size[1:i],y1=result,y2=time_model, xname="Train size / digit", y1_name="Success", y2_name="Time [s]", plot_name)
	}
	
	return(list(success=result,time_mod=time_model,time_pre=time_predict))
}

train_size <- seq(50,400,25)

if ( file.exists(fileName) && 0 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {
	plot_name = "../../../Report/graphics/nn_timing_trainsize_200.eps"
	plot_data = nn_time_vs_train(data, train_size, plot_name)
	result = plot_data$success
	time_model = plot_data$time_mod
	time_predict = plot_data$time_pre
}
dual_plot(x=train_size,y1=result,y2=time_model, xname="Train size / digit", y1_name="Success", y2_name="Time [s]", plot_name="Rplot.eps")



