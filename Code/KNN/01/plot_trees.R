library("C50")
source("load_people_data.R")
source("pca_test.R")
data = prepareOne(group=3,member=1,trainPart=360,testPart=40)
data = pca_simplification(data,noPC=50)

model = C50::C5.0(data$trainSet, as.factor(data$trainVali))
# summary( model )

ls(model)
print(model$rules)
# plot(model,subtree = 24)
# print(model)