# 87m18.371s runtime
source("main.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
m = 2
split = 0.5
no_of_tests = 1;

time = array(,3);
dpi_runs = 3
if( TRUE ) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, filename=paste(c("../../../Report/graphics/confusion_",dpi[i]),collapse=""))
		time[i] = data$time
		print(time)
	}
} else {
	# IMPORTANT: 
	# on a 50/50 split you have 200 comparisons 200 times = 40000 comparisons
	# on a 90/10 split you have 360 comparisons 040 times = 14400 comparisons
	# that is 2.77 times more comparisons, which tke plenty time!
# 	time =  c(87.484,230.587,509.792) #90/10 split
	time =  c(466.167,1304.835,3081.865) #50/50 split ... that can't be right...
	# time = c(100.12, 267.45, 545.37) # 90/10 split on Lukas pc
	# time = c(283.17, 752.38, 1519.85) # 50/50 split on Lukas pc
}

setEPS()
postscript("../../../Report/graphics/time_vs_dpi.eps",height = 4, width = 8)
plot(dpi,time,type="b",xlab="DPI",ylab="time [s]",ylim=(c(80,510)),col="black")
q = dev.off()