source("given_functions.R")

# getwd()
# setwd()

dpi = 1    # dpi
split = 0.5 # split tEst

p0 = c(1, 1)
# p1 = c(1, 2)
p2 = c(1, 3)
p3 = c(2, 1)
p4 = c(2, 2)
p5 = c(2, 3)
p6 = c(3, 1)
p7 = c(3, 2)
p8 = c(4, 1)
p9 = c(4, 2)
p10 = c(4, 3)
p11 = c(5, 1)
p12 = c(5, 2)
p13 = c(6, 1)
p14 = c(6, 2)
p15 = c(7, 1)
# p16 = c(7, 2)
p17 = c(7, 3)
all = c(p0, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p17)
# all = c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
test_people  = c(p6, p7)
train_people = p7
# k = 1:40

KNN_test(d, p6, p7, s, 10)
