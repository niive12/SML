library("gplots") # contour plot colors
fileName <- "nn_contour_pca_vs_size.RData"
load(fileName)

PC = seq(0,300,100)
size <- seq(100,400,100)
PC[1] = 5

result[4,1] = 0.75725
result[4,2] = 0.755
result[4,3] = 0.755
result[4,4] = 0.755

time_model[4,1] = 9898.86900
time_model[4,2] = 13598.325
time_model[4,3] = 16998
time_model[4,4] = 20398

time_predict[4,1] = 5839.30600
time_predict[4,2] = 7267.759
time_predict[4,3] = 8695
time_predict[4,4] = 10125


plot_name1 = "../../../Report/graphics/contour_nn_size_vs_pca.eps"
plot_name2 = "../../../Report/graphics/contour_nn_size_vs_pca_model.eps"
plot_name3 = "../../../Report/graphics/contour_nn_size_vs_pca_predict.eps"


setEPS()
postscript(plot_name1,height = 6, width = 8)
filled.contour(y = size, x = PC,result, col=colorpanel(20, "black", "white"), levels=seq(min(result), max(result), length.out= 21))
title(main = NULL, xlab = "no. PC", ylab = "No. hidden layers")
dev.off()

setEPS()
postscript(plot_name2,height = 6, width = 8)
filled.contour(y = size, x = PC,time_model, col=colorpanel(20, "black", "white"), levels=seq(min(time_model), max(time_model), length.out= 21))
title(main = NULL, xlab = "no. PC", ylab = "No. hidden layers")
dev.off()

setEPS()
postscript(plot_name3,height = 6, width = 8)
filled.contour(y = size, x = PC,time_predict, col=colorpanel(20, "black", "white"), levels=seq(min(time_predict), max(time_predict), length.out= 21))
title(main = NULL, xlab = "no. PC", ylab = "No. hidden layers")
dev.off()