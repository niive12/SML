
# the data
contour_data = array(1,c(10,5))
contour_data[4:9,2:4] = 2
contour_data[6:7,3] = 5

# actual measured points in ascending order
trainingSetSize = c(100,200,300,400,500,600,700,800,900,1000)
k = c(1,5,10,15,20)

# plot the contour plot
filled.contour(y = k, x = trainingSetSize, contour_data, color.palette = heat.colors)
title(main = NULL, xlab = "Training Array Size", ylab = "K")