saturn = read.csv("data.csv");
color = c("black","blue")

setEPS()
postscript("distance.eps",height = 4, width = 8)
plot(saturn[,1],saturn[,2],type="l",xlab="time [earth days]",ylab="distance [km]",ylim=c(min(saturn[,2:3]),max(saturn[,2:3])),col=color[1])
# plot(saturn[,1],diff,type="l",xlab="time [jorddøgn]",ylab="distance [km]",ylim=c(min(diff),max(diff)),col=color[1])
lines(saturn[,1],saturn[,3],type="l",col=color[2])
legend("topleft",legend=c("moon1", "moon2"),cex=0.8,col=color,lty=array(1,2))
q = dev.off()


y_lab = 0:2
y_lab[2] = "pi/2"
y_lab[3] = "pi"
setEPS()
postscript("angle.eps",height = 4, width = 8)
plot(saturn[,1],saturn[,4],type="l",xlab="time [earth days]",ylab="angle [rad]",ylim=c(min(saturn[,4]),max(saturn[,4])),col=color[1], yaxt="n")
axis(2, at=seq(0,pi,length=3), labels=y_lab)
q = dev.off()
