source("main.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
split1 = 0.9
split2 = 0.5

#each test is the same so variance will always be 0
no_of_tests = 10;

mean = data.frame(G3M2_9=c(0,0,0)); 
 var = data.frame(G3M2_9=c(0,0,0));
dpi_runs = 3
if( dpi_runs > 0) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split2, g, 1, ktest, no_of_tests)
		mean$G3M1_9[i] = data$mean;
		 var$G3M1_9[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split2, g, 1, ktest, no_of_tests)
		mean$G3M1_5[i] = data$mean
		 var$G3M1_5[i] = data$var;
	}                           
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split1, g, 2, ktest, no_of_tests)
		mean$G3M2_9[i] = data$mean;
		 var$G3M2_9[i] = data$var;
	}
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split1, g, 2, ktest, no_of_tests)
		mean$G3M2_5[i] = data$mean
		 var$G3M2_5[i] = data$var;
	}                           
} else {
	mean$G3M2_9= c(0.82975,0.86350,0.89250)
	mean$G3M2_9= c(0.82975,0.86350,0.89250)
	mean$G3M2_5= c(0.86000,0.89500,0.92000) 
	mean$G3M2_5= c(0.85450,0.89975,0.91900)
	mean$G3M1_9= c(0.77500,0.86025,0.88425)
	mean$G3M1_9= c(0.77500,0.86025,0.88425)
	mean$G3M1_5= c(0.77500,0.86025,0.88425)
	mean$G3M1_5= c(0.77500,0.86025,0.88425)
	
	var$G3M2_9= c(0.82975,0.86350,0.89250)
	var$G3M2_9= c(0.82975,0.86350,0.89250)
	var$G3M2_5= c(0.86000,0.89500,0.92000) 
	var$G3M2_5= c(0.85450,0.89975,0.91900)
	var$G3M1_9= c(0.77500,0.86025,0.88425)
	var$G3M1_9= c(0.77500,0.86025,0.88425)
	var$G3M1_5= c(0.77500,0.86025,0.88425)
	var$G3M1_5= c(0.77500,0.86025,0.88425)
}
print("Cross test is done")
print(mean) 
print(var)
colors = rainbow(4)
cn = c("G3M1 90/10","G3M1 50/50","G3M2 90/10","G3M2 50/50")
setEPS()
postscript("../../../Report/graphics/cross_test.eps",height = 4, width = 8) 
for(d in 1:4){
	if(d == 1) {
		plot(dpi,mean[[d]],type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.5,1)),col=colors[d])
	} else {
		lines(dpi,mean[[d]],type="b",lty=d, col=colors[d])
	}
}
legend(100,1,cn,cex=0.8,col=colors,lty=1:5)
q = dev.off()
write.latex(format(mean,digits=4), cn, dpi, "../../../Report/graphics/cross_mean.tex")
write.latex(format(mean,digits=2), cn, dpi, "../../../Report/graphics/cross_var.tex")