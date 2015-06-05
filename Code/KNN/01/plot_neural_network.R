source("load_people_data.R")
source("normalize.R")
source("neural_network_simplified.R")
source("pca_test.R")
library("devtools")
source("nnet_github.R")

fileName = "visualise_model.RData"

if ( file.exists(fileName) && 1 ) {
	print(paste(c("test data exists in ", fileName),collapse=""))
	load(fileName)
} else {

	load_setting = list(trainPartSize = 50, testSize = 10)
	normalize_setting = list(normMethod = "z-score")
	pca_setting         = list(noPC=5)

	n_hidden_layers = 10
	data <- prepareOneAloneNormPCA(3,2,load_setting,normalize_setting,pca_setting)

	model = neural_network_simplification(data,size=n_hidden_layers)
	save(model, file=fileName)
}
setEPS()
postscript("../../../Report/graphics/neural_network_visualized.eps",height = 4, width = 8)
par(mar=c(0, 4, 0, 4))
plot.nnet(model[[1]])
dev.off()