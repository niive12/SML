#result100 =
#[0.715,0.8,0.8175,0.855,0.855,0.8325,0.83,0.8425,0.84,0.8475];% tested
#with k = 10, test lists = 20
#result100 =
#[0.8,0.855,0.8325,0.8425,0.8475,0.8175,0.825,0.795,0.82,0.6575]; % tested
#with k = 10, 90/10, troels


#----------- 100 dpi ---------------
da = c(0.9475,0.9375,0.9675,0.9500,0.9625,0.9750,0.9500,0.9750,0.9475,0.9675); # tested with k = 10, 90/10, G3M2
db = c(0.9390,0.9425,0.9345,0.9320,0.9330,0.9425,0.9525,0.9460,0.9425,0.9385); # tested with k = 10, 50/50, G3M2
dc = c(0.9550,0.9540,0.9500,0.9540,0.9475,0.9550,0.9555,0.9545,0.9490,0.9550); # tested with k = 1, 50/50, G3M2
#----------- 200 dpi ---------------
dd = c(0.9475,0.9600,0.9500,0.9600,0.9625,0.9700,0.9575,0.9800,0.9575,0.9625); # tested with k = 10, 90/10, G3M2
de = c(0.9345,0.9360,0.9210,0.9265,0.9190,0.9340,0.9440,0.9480,0.9485,0.9400); # tested with k = 10, 50/50, G3M2
df = c(0.9580,0.9625,0.9565,0.9535,0.9505,0.9615,0.9675,0.9650,0.9665,0.9000); # tested with k = 1, 50/50, G3M2
#------------300 dpi--------------
# dg = c(0.8550,0.8875,0.8650,0.8750,0.8750,0.8500,0.8000,0.7800,0.7375,0.5100); # tested with k = 10, 90/10, troels
dh = c(0.9150,0.9400,0.9400,0.9550,0.9625,0.9475,0.9525,0.9650,0.9450,0.9525); # tested with k = 10, 90/10, G3M2
di = c(0.9160,0.9135,0.8990,0.9040,0.9015,0.9260,0.9325,0.9445,0.9325,0.9185); # tested with k = 10, 50/50, G3M2
dj = c(0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000,0.9000); # tested with k = 1, 50/50, G3M2

fake_timing = 999
# data_g3m2 = read.csv(file=paste(c("result_G",3,"M",2,".csv"),collapse=""))
data_g3m2 = data.frame(append(fake_timing,da,1));       names(data_g3m2 ) = c("K10_S90_D100")
data_g3m2$K1_S50_D100  = append(fake_timing,dc,1)
data_g3m2$K10_S50_D100 = append(fake_timing,db,1)
data_g3m2$K10_S90_D200 = append(fake_timing,dd,1)
data_g3m2$K1_S50_D200  = append(fake_timing,df,1)
data_g3m2$K10_S50_D200 = append(fake_timing,de,1)
data_g3m2$K10_S90_D300 = append(fake_timing,dh,1)
data_g3m2$K10_S50_D300 = append(fake_timing,di,1)
data_g3m2$K1_S50_D300  = append(fake_timing,dj,1)
write.csv(data_g3m2, file=paste(c("result_G",3,"M",2,".csv"),collapse=""), row.names=FALSE)