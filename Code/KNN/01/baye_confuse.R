# outputs the confusion table for the best setting of baye

source("load_people_data.R")
source("pca_test.R")
source("normalize.R")
source("Baye.R")


PCA <- 0.5
bins <- 50


data <- prepareOneAlone(3,2, 400, 400)

# reduce data (pca)
data <- normalizeData(data, "z-score")
data <- pca_simplification(data, breakpoint=PCA)

# bin
data <- normalizeData(data, "bin", bins = bins)

# z-score, PCA, Bayes
result <- (baye_predict(data))$confus

write.latex(result, 0:9, 0:9, "baye_confusionmatrix.tex")
