### Function to create volcano plot
volcanoPlot <- function(x,p.value=p.value,Difference=Difference,pval_threshod=0.05,sep="\t",dec=".",
                        output="volcanoplot.tiff",width=8,height=8,units="in",res=300){
  ## Reading file
  x = read.csv(x,sep=sep,header=TRUE,dec=dec)
  ## Creating tiff output
  tiff(filename = output,height=height,width=width,units=units,res=res)
  xlim = c(-abs(max(x$Difference)),abs(max(x$Difference)))
  plot(x=x$Difference,y=-log10(x$p.value),pch=20,cex=1,col="Grey",xlim=xlim,xlab="Mean.Difference",ylab="-log10(p)")
  with(subset(x,p.value <= pval_threshod & abs(Difference) < 1),points(x=Difference,y=-log10(p.value),pch=20,cex=1.5,col="LightBlue"))
  with(subset(x,p.value <= pval_threshod & Difference <= -1),points(x=Difference,y=-log10(p.value),pch=20,cex=1.5,col="LightGreen"))
  with(subset(x,p.value <= pval_threshod & Difference >= 1),points(x=Difference,y=-log10(p.value),pch=20,cex=1.5,col="Pink"))
  with(subset(x,p.value <= pval_threshod),points(x=Difference,y=-log10(p.value),pch=20,cex=1.5,col="Blue"))
  with(subset(x,p.value <= pval_threshod & Difference >= 1),points(x=Difference,y=-log10(p.value),pch=20,cex=2,col="Red"))
  with(subset(x,p.value <= pval_threshod & Difference <= -1),points(x=Difference,y=-log10(p.value),pch=20,cex=2,col="DarkGreen"))
  abline(v=c(-1,1),h=c(-log10(pval_threshod),-log10(0.01)),lwd=1,lty=3)
  dev.off()
  ## closed 
}


