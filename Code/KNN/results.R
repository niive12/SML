#----------- 100 dpi ---------------
#result100 =
#[0.715,0.8,0.8175,0.855,0.855,0.8325,0.83,0.8425,0.84,0.8475];% tested
#with k = 10, test lists = 20
#result100 =
#[0.8,0.855,0.8325,0.8425,0.8475,0.8175,0.825,0.795,0.82,0.6575]; % tested
#with k = 10, 90/10, troels
# 	result100_9010_10 =   c(0.9475,0.9375,0.9675,0.9500,0.9625,0.9750,0.9500,0.9750,0.9475,0.9675); # tested with k = 10, 90/10, G3M2
# 	result100_5050_10 =   c(0.9390,0.9425,0.9345,0.9320,0.9330,0.9425,0.9525,0.9460,0.9425,0.9385); # tested with k = 10, 50/50, G3M2
# 	result100_5050_1  =   c(0.9550,0.9540,0.9500,0.9540,0.9475,0.9550,0.9555,0.9545,0.9490,0.9550); # tested with k = 1,  50/50, G3M2
result_G3M2       = read.csv("result_G3M2.csv");
mean100_9010      = mean(tail(result_G3M2$K10_S90_D100,10) );
var100_9010       = var( tail(result_G3M2$K10_S90_D100,10) );
mean100_5050      = mean(tail(result_G3M2$K10_S50_D100,10) );
var100_5050       = var( tail(result_G3M2$K10_S50_D100,10) );
mean100_5050_1    = mean(tail(result_G3M2$K1_S50_D100,10)  );
var100_5050_1     = var( tail(result_G3M2$K1_S50_D100,10)  );
#----------- 200 dpi ---------------
# 	result200 = c(); # tested with k = 10, 90/10, troels
# 	result200_9010   = c(0.9475,0.9600,0.9500,0.9600,0.9625,0.9700,0.9575,0.9800,0.9575,0.9625);  # tested with k = 10, 90/10, G3M2
# 	result200_5050   = c(0.9345,0.9360,0.9210,0.9265,0.9190,0.9340,0.9440,0.9480,0.9485,0.9400);  # tested with k = 10, 50/50, G3M2
# 	result200_5050_1 = c(0.9580,0.9625,0.9565,0.9535,0.9505,0.9615,0.9675,0.9650,0.9665,0.9000);  # tested with k = 1, 50/50,  G3M2
mean200_9010   = mean(tail(result_G3M2$K10_S90_D200,10) );
var200_9010    = var( tail(result_G3M2$K10_S90_D200,10) );
mean200_5050   = mean(tail(result_G3M2$K10_S50_D200,10) );
var200_5050    = var( tail(result_G3M2$K10_S50_D200,10) );
mean200_5050_1 = mean(tail(result_G3M2$K1_S50_D200,10)  );
var200_5050_1  = var( tail(result_G3M2$K1_S50_D200,10)  );
#------------300 dpi--------------
# 	result300        = c(0.8550,0.8875,0.8650,0.8750,0.8750,0.8500,0.8000,0.7800,0.7375,0.5100); # tested with k = 10, 90/10, troels
# 	result300_9010   = c(0.9150,0.9400,0.9400,0.9550,0.9625,0.9475,0.9525,0.9650,0.9450,0.9525); # tested with k = 10, 90/10, G3M2
# 	result300_5050   = c(0.9160,0.9135,0.8990,0.9040,0.9015,0.9260,0.9325,0.9445,0.9325,0.9185); # tested with k = 10, 50/50, G3M2
# 	result300_5050_1 = c(0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000); # tested with k = 1, 50/50,  G3M2
mean300_9010   = mean(tail(result_G3M2$K10_S90_D300,10) );
var300_9010    = var( tail(result_G3M2$K10_S90_D300,10) );
mean300_5050   = mean(tail(result_G3M2$K10_S50_D300,10) );
var300_5050    = var( tail(result_G3M2$K10_S50_D300,10) );
mean300_5050_1 = mean(tail(result_G3M2$K1_S50_D300,10)  );
var300_5050_1  = var( tail(result_G3M2$K1_S50_D300,10)  );
#
x        = c(100,200,300);
y_9010   = c(mean100_9010,mean200_9010,mean300_9010);
y_5050   = c(mean100_5050,mean200_5050,mean300_5050);
y_5050_1 = c(mean100_5050_1,mean200_5050_1,mean300_5050_1);


colors = rainbow(3)
setEPS()
postscript("graph.eps",height = 4, width = 8)
plot(x,y_9010,type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.9,0.98)),col=colors[1])
lines(x,y_5050,type="b",lty=2, col=colors[2])
lines(x,y_5050_1,type="b", lty=3, col=colors[3])
legend(100,0.93,c("90:10","50:50","50:50"),cex=0.8,col=colors,lty=1:3)
dev.off()
