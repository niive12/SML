# make function that plots data with variance
library("Hmisc")

# plots the means given a variance
# mean       : matrix with rows = lines plots, column data points
# var        : same as matrix
# x          : place to plot it on x-axis
# xname      : axis labels of the plot points
# xlab       : x-axis label
# ylab       : y-axis label
# filename   : place and name of where to store
# legendname : name of the lines
plot_wVar <- function(y, yvar, x = 1:(length(y[1,])), xname = 1:(length(y[1,])), xlab = "x", ylab = "y", filename = "plot_wVar.eps", legendopt = c(), legpos = "bottomright"){
	datasets = dim(y)[1]
	if(is.matrix(y)){
		color = rainbow(datasets)
		setEPS()
		# save dest
		postscript(filename, height = 5, width = 8)
		# plot
		plot(x, y[1,], type = "b", col = color[1], ylim = c(min(y - yvar), max(y + yvar)), xlab = xlab, ylab = ylab, xaxt = "n")
		# add error bars
		errbar(x = x, y = y[1,], y[1,] + yvar[1,], y[1,] - yvar[1,], add = T, type="n")
		# add axis
		axis(1, at = x, labels = xname)
		# add legends
		if(length(legendopt) > 0){
			legend(legpos, legendopt, cex=0.8, lty = 1, col = color)
		}
		# add other plot lines, if they exist
		if(datasets > 1){
			for(i in 2:datasets){
				lines(x, y[i,], type = "b", col = color[i])
				errbar(x, y[i,], y[i,] + yvar[i,], y[i,] - yvar[i,], add = T, type="n")
			}
		}
		q = dev.off()
		
	}
}

m = matrix(1:10,2,5)
v = matrix((1:10)/10,2,5)
xaxis = c("g1","g3","g4","g2","g5")
lab = c("Smoothed","Not Smoothed")

plot_wVar(y = m, yvar = v, xname = xaxis, xlab = "Group and Member", ylab = "Success")