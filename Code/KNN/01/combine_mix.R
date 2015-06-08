load("tree_performance_mix.RData")
success_p = success[1:10]
load("tree_performance_mix2.RData")
success_s = success
names=c("prepared","smoothed")
setEPS()
postscript("../../../Report/graphics/tree_performance_mix_combined.eps",height = 4, width = 8)
par(mar=c(3, 4, 4, 1) + 0.1)
boxplot(success_p,success_s,ylab="Success Rate", outline = FALSE,names=names) 
dev.off()

print(paste(c("Mean / Var of the easy: ", mean(success_p), " / ", var(success_p)), collapse = ""))
print(paste(c("Mean / Var of the easy: ", mean(success_s), " / ", var(success_s)), collapse = ""))