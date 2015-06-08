library("gplots") # for colorpanel

success_hard_tree = c(0.51300, 0.76300, 0.66750, 0.49500, 0.73350, 0.77150, 0.79700, 0.89150, 0.88325, 0.74575, 0.59850, 0.81900, 0.86525, 0.84750, 0.77025, 0.49975, 0.87900, 0.56250, 0.81900, 0.66875)

load("KNN_final_full_test.RData")
success_hard_knn = success_hard


# plot
x_lab <- 1:noPeople
for(i in 1:noPeople){
	x_lab[i] <- paste(c(people[[i]][1],":",people[[i]][2]),collapse="")
}

color = rainbow(2)
setEPS()
postscript("../../../Report/graphics/success_comp_hard.eps",height = 6, width = 8)
par(mar=c(3.8, 4.5, 1, 4.5))
plot(1:noPeople,success_hard_tree, xaxt="n",type="b",xlab="Person",ylab="Success Rate", col = color[1], ylim = c(min(success_hard_tree,success_hard_knn),max(success_hard_tree,success_hard_knn))) 
lines(1:noPeople,success_hard_knn,type="b", col = color[2])
abline(h=mean(success_hard_tree), col = color[1], lty = "dashed")
abline(h=mean(success_hard_knn), col = color[2], lty = "dashed")
axis(1, at=1:noPeople, labels=x_lab, las = 2)
q = dev.off()
