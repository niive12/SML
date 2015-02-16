#----------- 100 dpi ---------------
#result100 =
#[0.715,0.8,0.8175,0.855,0.855,0.8325,0.83,0.8425,0.84,0.8475];% tested
#with k = 10, test lists = 20
#result100 =
#[0.8,0.855,0.8325,0.8425,0.8475,0.8175,0.825,0.795,0.82,0.6575]; % tested
#with k = 10, 90/10, troels
result100_9010 =   c(0.9475,0.9375,0.9675,0.95,0.9625,0.975,0.95,0.975,0.9475,0.9675);   # tested with k = 10, 90/10, G3M2
result100_5050 =   c(0.939,0.9425,0.9345,0.932,0.933,0.9425,0.9525,0.946,0.9425,0.9385); # tested with k = 10, 50/50, G3M2
result100_5050_1 = c(0.955,0.954,0.95,0.954,0.9475,0.955,0.9555,0.9545,0.949,0.955);     # tested with k = 1, 50/50, G3M2
mean100_9010 = mean(result100_9010);
var100_9010 = var(result100_9010);
mean100_5050 = mean(result100_5050);
var100_5050 = var(result100_5050);
mean100_5050_1 = mean(result100_5050_1);
var100_5050_1 = var(result100_5050_1);
#----------- 200 dpi ---------------
result200 = c(); # tested with k = 10, 90/10, troels
result200_9010   = c(0.9475,0.96,0.95,0.96,0.9625,0.97,0.9575,0.98,0.9575,0.9625);    # tested with k = 10, 90/10, G3M2
result200_5050   = c(0.9345,0.936,0.921,0.9265,0.919,0.934,0.944,0.948,0.9485,0.94);  # tested with k = 10, 50/50, G3M2
result200_5050_1 = c(0.958,0.9625,0.9565,0.9535,0.9505,0.9615,0.9675,0.965,0.9665);   # tested with k = 1, 50/50, G3M2
mean200_9010 = mean(result200_9010);
var200_9010 = var(result200_9010);
mean200_5050 = mean(result200_5050);
var200_5050 = var(result200_5050);
mean200_5050_1 = mean(result200_5050_1);
var200_5050_1 = var(result200_5050_1);
#------------300 dpi--------------
result300        = c(0.855,0.8875,0.865,0.875,0.875,0.85,0.8,0.78,0.7375,0.51);          #
#tested with k = 10, 90/10, troels
result300_9010   = c(0.915,0.94,0.94,0.955,0.9625,0.9475,0.9525,0.965,0.945,0.9525);     # tested with k = 10, 90/10, G3M2
result300_5050   = c(0.916,0.9135,0.899,0.904,0.9015,0.926,0.9325,0.9445,0.9325,0.9185); # tested with k = 10, 50/50, G3M2
result300_5050_1 = c(0.9);                                                               # tested with k = 1, 50/50, G3M2
mean300_9010 = mean(result300_9010);
var300_9010 = var(result300_9010);
mean300_5050 = mean(result300_5050);
var300_5050 = var(result300_5050);
mean300_5050_1 = mean(result300_5050_1);
var300_5050_1 = var(result300_5050_1);
#
# figure(1)
x        = c(100,200,300);
y_9010   = c(mean100_9010,mean200_9010,mean300_9010);
y_5050   = c(mean100_5050,mean200_5050,mean300_5050);
y_5050_1 = c(mean100_5050_1,mean200_5050_1,mean300_5050_1);


colors = rainbow(3)
setEPS()
postscript("graph.eps",horizontal = FALSE, paper = "special",height = 4, width = 8)
plot(x,y_9010,type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.9,0.98)),col=colors[1])
lines(x,y_5050,type="b",lty=2, col=colors[2])
lines(x,y_5050_1,type="b", lty=3, col=colors[3])
legend(100,0.93,c("90:10","50:50","50:50"),cex=0.8,col=colors,pch=21:22,lty=1:3)
dev.off()
#,y_5050,y_5050_1
# xlabel('Image Resolution (dpi)')
# ylabel('Mean Succes Rate')
