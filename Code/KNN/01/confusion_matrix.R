# columns are actual and hence sum = 1
# rows are predicted
confusion_matrix <- function(input, print_as_procent=T, filename="confusion.eps"){
	colnames(input) = 0:9;
	rownames(input) = 0:9;
	lwid = c(1.5,4)
	lhei = c(1.5,4,1)
	lmat = rbind(c(0,3),c(2,1),c(0,4))
	
	if(print_as_procent){
		tot = sum(input[,1])
		input = round(input/tot,2)
	}
	
	setEPS()
	postscript(filename,height = 8, width = 8)
	heatmap.2(input
		,dendrogram="none"
		,col=colorpanel(100, "orange","red")
		,trace="none"
		,Colv=FALSE
		,Rowv=FALSE
		,cexRow=1.9
		,cexC=1.9
		,key=FALSE
		,lmat=rbind(c(0,2),c(0,3),c(0,1),c(0,4))
		,lhei=c(0.09,0.09,0.81,0.01)
		,lwid=c(0.1,1)
		,notecex=2
		,notecol="black"
		,cellnote=input)
	mtext("Predicted Class", side=2, line=2,cex=2)
	mtext("Actual Class", side=3, line=-2, cex=2)
	q = dev.off()
}