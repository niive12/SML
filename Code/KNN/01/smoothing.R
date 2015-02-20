source("main.R") 
ktest = 10;
dpi = c(100,200,300)

#each test is the same so variance will always be 0
no_of_tests = 1;

mean = data.frame(raw=c(0,0,0)); 
var = data.frame(raw=c(0,0,0));
dpi_runs = 1
if( dpi_runs > 0) {
	for(i in 1:dpi_runs){
		res = array(,no_of_tests)
		for(n in 1:no_of_tests){
			data = KNN_test_one_person(i, 0.9, 3, 2, ktest, 1)
			res[n] = data$mean;
		}
		mean$raw[i] = mean(res);
		var$raw[i] = var(res);
	}
	for(i in 1:dpi_runs){
		res = array(,no_of_tests)
		for(n in 1:no_of_tests){
			data = KNN_test_one_person(i, 0.9, 3, 2, ktest, 1, smooth="avarage")
			res[n] = data$mean;
		}
		mean$avg[i] = mean(res);
		var$avg[i] = var(res);
	}                           
	for(i in 1:dpi_runs){
		res = array(,no_of_tests)
		for(n in 1:no_of_tests){
			data = KNN_test_one_person(i, 0.9, 3, 2, ktest, 1, smooth="gaussian",sigma=0.5)
			res[n] = data$mean;
		}
		mean$gap5[i] = mean(res);
		var$gap5[i] = var(res);
	}
	for(i in 1:dpi_runs){
		res = array(,no_of_tests)
		for(n in 1:no_of_tests){
			data = KNN_test_one_person(i, 0.9, 3, 2, ktest, 1, smooth="gaussian",sigma=1)
			res[n] = data$mean;
		}
		mean$ga1[i] = mean(res);
		var$ga1[i] = var(res);
	}                         
	for(i in 1:dpi_runs){
		res = array(,no_of_tests)
		for(n in 1:no_of_tests){
			data = KNN_test_one_person(i, 0.9, 3, 2, ktest, 1, smooth="gaussian",sigma=2)
			res[n] = data$mean;
		}
		mean$ga2[i] = mean(res);
		var$ga2[i] = var(res);
	}
} else {
	mean$raw =  c(0.82975,0.86350,0.89250)
	mean$avg =  c(0.82975,0.86350,0.89250)
	mean$gap5 = c(0.86000,0.89500,0.92000)     #only 1 test...
	mean$ga1 =  c(0.85450,0.89975,0.91900)
	mean$ga2 =  c(0.77500,0.86025,0.88425)
}
print(mean) 
print(var)
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
write.latex(format(mean,digits=2), cn, dpi, "../../../Report/graphics/smoothing_var.tex")