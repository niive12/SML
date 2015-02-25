# 11h35m43.098s run time
source("part01.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
m = 2
split = 0.9
#each test is the same so variance will always be 0
no_of_tests = 10;

mean = data.frame(raw=c(0,0,0)); 
var  = data.frame(raw=c(0,0,0));
dpi_runs = 3
if( FALSE) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests)
		mean$raw[i] = data$mean;
		var$raw[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, smooth="avarage")
		mean$avg[i] = data$mean
		var$avg[i] = data$var;
	}                           
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, smooth="gaussian",sigma=0.5)
		mean$gap5[i] = data$mean
		var$gap5[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, smooth="gaussian",sigma=1)
		mean$ga1[i] = data$mean
		var$ga1[i] = data$var;
	}                         
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, smooth="gaussian",sigma=2)
		mean$ga2[i] = data$mean
		var$ga2[i] = data$var;
	}
} else {
	mean$raw =  c(0.82975,0.86350,0.89250)
	mean$avg =  c(0.82975,0.86350,0.89250)
	mean$gap5 = c(0.87875,0.91800,0.93725)     
	mean$ga1 =  c(0.85450,0.89975,0.91900)
	mean$ga2 =  c(0.77500,0.86025,0.88425)
	
	#haven't meassured yet
	var$raw =  c(0.0119325694444444,0.0110044444444444,0.00551527777777778)
	var$avg =  c(0.0119325694444444,0.0110044444444444,0.00551527777777778)
	var$gap5 = c(0.00622256944444444,0.00523861111111111,0.00274090277777778)     #only 1 test...
	var$ga1 =  c(0.00969833333333333,0.00440618055555556,0.00313361111111111)
	var$ga2 =  c(0.0179736111111111,0.00795618055555556,0.00587784722222222)
}
print(mean) 
  var = var
print(format(var,digits=2))
colors = rainbow(5)
setEPS()
postscript("../../../Report/graphics/smoothing.eps",height = 4, width = 8)# 
for(d in 1:5){
	if(d == 1) {
		plot(dpi,mean[[d]],type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.7,1)),col=colors[d])
	} else {
		lines(dpi,mean[[d]],type="b",lty=d, col=colors[d])
	}
}
legend(261,0.85,c("raw","average","g sigma=0.5","g sigma=1","g sigma=2"),cex=0.8,col=colors,lty=1:5)
q = dev.off()
cn = c("Raw","Avg","G 0.5","G 1","G 2")
write.latex(format(mean,digits=4), cn, dpi, "../../../Report/graphics/smoothing_mean.tex")
write.latex(format(var,digits=2), cn, dpi, "../../../Report/graphics/smoothing_var.tex")