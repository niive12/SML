# library("gplots") # needs to be installe

source("load_people_data.R")
source("pca_test.R")

# all-mixed (100 DPI)
# time vs no. ppl. (90/10 split, fixed K = 10)

k = 10
split = 0.9

people <- getPeople()

for(ppl in 1:length(people)){

	
data <- prepareAllMixed(360,40,peopleToLoad = people[1:ppl)
startTime <- proc.time() # used for timing
# do stuff....

time <- ((proc.time() - startTime)[3])

}
