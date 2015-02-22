
source("main.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
split1 = 0.9
split2 = 0.5

#each test is the same so variance will always be 0
no_of_tests = 10;

mean = data.frame(G3M1_9=c(0,0,0)); 
 var = data.frame(G3M1_9=c(0,0,0));
dpi_runs = 3
if( FALSE) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split1, g, 1, ktest, no_of_tests)
		mean$G3M1_9[i] = data$mean;
		 var$G3M1_9[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split2, g, 1, ktest, 1)
		mean$G3M1_5[i] = data$mean
		 var$G3M1_5[i] = data$var;
	}                           
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split1, g, 2, ktest, no_of_tests)
		mean$G3M2_9[i] = data$mean;
		 var$G3M2_9[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split2, g, 2, ktest, 1)
		mean$G3M2_5[i] = data$mean
		 var$G3M2_5[i] = data$var;
	}                           
} else {
	mean$G3M1_9= c(0.68575,0.65525,0.62050) #10 times
	mean$G3M1_5= c(0.35700,0.35900,0.27350) #1 time
	mean$G3M2_9= c(0.82975,0.86350,0.89250) #10 times
	mean$G3M2_5= c(0.79350,0.79350,0.81050) #1 time
	
	var$G3M1_9= c(0.0260750694444444,0.0274561805555556,0.0398775)
	var$G3M1_5= c(0.02529,0.86025,0.0708447222222222)
	var$G3M2_9= c(0.0119325694444444,0.0110044444444444,0.00551527777777778)
	var$G3M2_5= c(0.0222947222222222,0.0222947222222222,0.0153525)
}
print("Cross test is done")
print(mean) 
# var = var*10
print(format(var,digits=3))
colors = rainbow(4)
cn = c("M1 90","M1 50","M2 90","M2 50")
setEPS()
postscript("../../../Report/graphics/cross_test.eps",height = 4, width = 8) 
for(d in 1:4){
	if(d == 1) {
		plot(dpi,mean[[d]],type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.25,0.9)),col=colors[d])
	} else {
		lines(dpi,mean[[d]],type="b",lty=d, col=colors[d])
	}
}
legend(270,0.6,cn,cex=0.8,col=colors,lty=1:4)
q = dev.off()
write.latex(format(mean,digits=4), cn, dpi, "../../../Report/graphics/cross_mean.tex")
write.latex(format(var,digits=3), cn, dpi, "../../../Report/graphics/cross_var.tex")