source("main.R") 
ktest = 10;
dpi = c(100,200,300)
g = 3
m = 2
split = 0.5
no_of_tests = 1;

time = array(,3);
dpi_runs = 3
if( dpi_runs > 0) {
	for(i in 1:dpi_runs){
		data = KNN_test_one_person(i, split, g, m, ktest, no_of_tests, filename=paste(c("../../../Report/graphics/confusion_",dpi[i]),collapse=""))
	}
}