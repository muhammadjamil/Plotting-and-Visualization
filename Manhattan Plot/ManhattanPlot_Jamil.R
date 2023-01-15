
rm(list=ls())

## Function for numerical conversion ##
asCN <- function(x){as.numeric(as.character(x))}
### Reading chromosome lengths ##
chrLengths = read.csv("hg38_length.txt",sep="\t",header=F,dec=".")

## reading correlation values matrix ##
x = read.csv("Example.txt",sep="\t",header=T,dec=",")
x = subset(x,!is.na(pvalue))


x$chr[which(x$chr=="X")] = "23"
x$chr[which(x$chr=="X")] = "24"

x = subset(x,chr %in% seq(1,24,1))

plot_manhattan <- function(x,cex_NS = 0.5,cex_S=2,chr_col="chr",p_col="pvalue",cor_col="estimate",
                           Neg_col="Red",Pos_col="Blue",main="",pth=0.01){
  colnames(x)[which(colnames(x) == chr_col)] = "chr"
  colnames(x)[which(colnames(x) == p_col)] = "pvalue"
  colnames(x)[which(colnames(x) == cor_col)] = "estimate"
  plot(x=0,y=0,xlim=c(0,sum(chrLengths$V2)),ylim=c(0,max(-log10(asCN(x$pvalue)))),bty="n",main=main,
       type="n",xaxt="n",yaxt="n",xlab="",ylab="-log10(p)")
  with(subset(x,chr=="1"),points(x=asCN(pos),y=-log10(pvalue),pch=20,cex=cex_NS,col="azure4"))
  with(subset(x,chr=="1" & pvalue <= pth & estimate <= 0),points(x=asCN(pos),y=-log10(pvalue),pch=20,cex=cex_S,col="Red"))
  with(subset(x,chr=="1" & pvalue <= pth & estimate >= 0),points(x=asCN(pos),y=-log10(pvalue),pch=20,cex=cex_S,col="Blue"))
  
  pos_X=1
  sumX = chrLengths$V2[1]/2
  for(i in 2:nrow(chrLengths)){
    pos_X = chrLengths$V2[i-1]+pos_X
    sumX = c(sumX,sumX[i-1]+(asCN(chrLengths$V2[i])/2) +(asCN(chrLengths$V2[i-1])/2))
    if(i %in% c(2,4,6,8,10,12,14,16,18,20,22,24))
      col="azure2"
    else
      col="azure4"
    with(subset(x,chr==i),points(x=asCN(pos)+pos_X,y=-log10(pvalue),pch=20,cex=cex_NS,col=col))
    with(subset(x,chr==i & pvalue <= pth & estimate <= 0),points(x=asCN(pos)+pos_X,y=-log10(pvalue),pch=20,cex=cex_S,col=Neg_col))
    with(subset(x,chr==i & pvalue <= pth & estimate >= 0),points(x=asCN(pos)+pos_X,y=-log10(pvalue),pch=20,cex=cex_S,col=Pos_col))
    
  }
  axis(2)
  axis(1,at=sumX,labels = c(seq(1,22,1),"X","Y"))
  abline(h=-log10(pth),lty=3,lwd=2)
  
}
tiff("manhattan jamil.tiff",width=24,height=11,units="in",res=150)
plot_manhattan(x,cex_NS=1,cex_S=4,p_col="p.value",pth=0.01)
dev.off()
