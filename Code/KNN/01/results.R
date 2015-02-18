
result_G3M2       = read.csv("result_G3M2.csv");
mean100_9010      = mean(tail(result_G3M2$K10_S90_D100,10) );
var100_9010       = var( tail(result_G3M2$K10_S90_D100,10) );
mean100_5050      = mean(tail(result_G3M2$K10_S50_D100,10) );
var100_5050       = var( tail(result_G3M2$K10_S50_D100,10) );
mean100_5050_1    = mean(tail(result_G3M2$K1_S50_D100,10)  );
var100_5050_1     = var( tail(result_G3M2$K1_S50_D100,10)  );


#----------- 200 dpi ---------------
mean200_9010   = mean(tail(result_G3M2$K10_S90_D200,10) );
var200_9010    = var( tail(result_G3M2$K10_S90_D200,10) );
mean200_5050   = mean(tail(result_G3M2$K10_S50_D200,10) );
var200_5050    = var( tail(result_G3M2$K10_S50_D200,10) );
mean200_5050_1 = mean(tail(result_G3M2$K1_S50_D200,10)  );
var200_5050_1  = var( tail(result_G3M2$K1_S50_D200,10)  );

#------------300 dpi--------------
mean300_9010   = mean(tail(result_G3M2$K10_S90_D300,10) );
var300_9010    = var( tail(result_G3M2$K10_S90_D300,10) );
mean300_5050   = mean(tail(result_G3M2$K10_S50_D300,10) );
var300_5050    = var( tail(result_G3M2$K10_S50_D300,10) );
mean300_5050_1 = mean(tail(result_G3M2$K1_S50_D300,10)  );
var300_5050_1  = var( tail(result_G3M2$K1_S50_D300,10)  );

x           = c(100,200,300);

y = data.frame(mean90=c(0,0,0)); 
# names(y) =  c("mean90")

y$mean90    =  c(mean100_9010,mean200_9010,mean300_9010); 
y$mean50    =  c(mean100_5050,mean200_5050,mean300_5050);
y$mean50_1  =  c(mean100_5050_1,mean200_5050_1,mean300_5050_1);

yv = data.frame(ar90=c(0,0,0));
yv$ar90     =  c(var100_9010,var200_9010,var300_9010); 
yv$ar50     =  c(var100_5050,var200_5050,var300_5050); 
yv$ar50_1   =  c(var100_5050_1,var200_5050_1,var300_5050_1); 

colors = rainbow(3)

setEPS()
postscript("graph.eps",height = 4, width = 8)
error_bar_width = 1; 
for(d in 1:3){
	if(d == 1) {
		plot(x,y[[d]],type="b",xlab="DPI",ylab="mean success rate",ylim=(c(0.9,0.98)),col=colors[d])
	} else {
		lines(x,y[[d]],type="b",lty=d, col=colors[d])
	}
# 	for(i in 1:3){ #variance error bar
# 		up = (y[[d]][i] + (yv[[d]][i])*3)
# 		low = (y[[d]][i] - (yv[[d]][i])*3)
# 		segments(x[i], low, x[i],up)
# 		segments(x[i]-error_bar_width, up , x[i]+error_bar_width, up)
# 		segments(x[i]-error_bar_width, low , x[i]+error_bar_width, low)
# 	}
}
legend(100,0.93,c("90:10","50:50","50:50"),cex=0.8,col=colors,lty=1:3)
dev.off()
