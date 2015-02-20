
# script for running the contour plots of K vs TrainSize for G3 M1 and 2

source("main.R")

# these tests take a LONG time, set traintest to (100,360,40) and getContours(ktest,traintest,1,3,2) to speed everyting up!

# vector values for K and trainSize
ktest = 1:40
traintest = c(seq(100,360,10))

# calc., plot and save the data
getContours(ktest,traintest,10,3,2)
print("First Done.")
getContours(ktest,traintest,10,3,1)