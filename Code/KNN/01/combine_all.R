source("confusion_matrix.R")
library("gplots")
library("graphics")

load("tree_performance_all.RData")
success_p = success
load("tree_performance_all2.RData")
success_s= array(0,length(people))
success_s[1:10] = success[1:10]
b = c(0.59850, 0.81900, 0.86525, 0.84750, 0.77025, 0.49975, 0.87900, 0.56250,
0.81900, 0.66875)
success_s[11:20] = b
confus_s = confus
# print(success_p)
print(success_s)

colors = c("red","blue")
setEPS()
postscript("../../../Report/graphics/tree_performance_all_combined.eps",height = 6, width = 8)
plot(1:length(people),success_p, xaxt="n",type="b",xlab="Person",ylab="Success Rate",col=colors[1], ylim=c(min(c(min(success_p),min(success_s))),max(c(max(success_p),max(success_s)))) ) 
abline(h=mean(success_p), col = colors[1],lty="dashed")
lines(1:length(people),success_s, type="b",col=colors[2]) 
abline(h=mean(success_s), col = colors[2],lty="dashed")
legend("bottomright",pch=c(16,16),legend=c("prepared","smoothed"),cex=0.8,col=colors)
axis(1, at=1:length(people), labels=x_lab, las=2)
dev.off()

load("tree_performance_all2_lukas.RData")
confus_s = confus_s + confus
confusion_matrix(confus, filename="../../../Report/graphics/tree_confusion_all2.eps")
# success_all = c(0.51300, 0.76300, 0.66750, 0.49500, 0.73350, 0.77150, 0.79700, 0.89150, 0.88325, 0.74575, 0.59850, 0.81900, 0.86525, 0.84750, 0.77025, 0.49975, 0.87900, 0.56250, 0.81900, 0.66875)

print(paste(c("Mean / Var of the hard: ", mean(success_p*100), " / ", var(success_p*100)), collapse = ""))
print(paste(c("Mean / Var of the hard: ", mean(success_s*100), " / ", var(success_s*100)), collapse = ""))