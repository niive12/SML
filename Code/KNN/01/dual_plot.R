dual_plot <- function(x,y1,y2, xname, y1_name, y2_name, plot_name="Rplot.eps"){
	colors= c("black","blue")
	len = length(x)
	y_1 = y1[1:len]
	y_2 = y2[1:len]
	setEPS()
	postscript(plot_name,height = 4, width = 8)
		# add margin so we can see both labels
		par(mar=c(5, 4, 4, 6) + 0.1)

		# plot time
		plot(x, y_1, pch=16, ylim=c(min(y_1),max(y_1)), col=colors[1],
		axes=FALSE, type="o", xlab="", ylab="")
		axis(2, ylim=c(0,1),col=colors[1],las=1)  # las=1 makes horizontal labels
		mtext(y1_name,side=2,line=2.5)
		box()

		par(new=TRUE)

		# plot success
		plot(x, y_2, pch=15, ylim=c(min(y_2),max(y_2)), col=colors[2],
		axes=FALSE, type="o", xlab="", ylab="")
		mtext(y2_name,side=4,col=colors[2],line=4) 
		axis(4, ylim=c(0,7000), col=colors[2],col.axis=colors[2],las=1)

		# Draw the axis
		axis(1,pretty(range(x),10))
		mtext(xname,side=1,col="black",line=2.5)  

		legend("bottomright",legend=c(y1_name,y2_name),pch=16:15,cex=0.8,col=colors)
	quiet = dev.off()
}