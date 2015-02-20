source("main.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
m = 2
split = 0.9
#each test is the same so variance will always be 0
no_of_tests = 1;

time = array(,3);
dpi_runs = 3
if( dpi_runs > 0) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests)
		time[i] = data$time
	}
} else {
	time =  c(87.484,230.587,509.792)
}
setEPS()
postscript("../../../Report/graphics/time_vs_dpi.eps",height = 4, width = 8)
plot(dpi,time,type="b",xlab="DPI",ylab="time [s]",ylim=(c(80,510)),col="black")
q = dev.off()