
X <- data.frame(TOTAL_B$xxmsy[1,],TOTAL_B_APRX$xxmsy[1,],
                TOTAL_B$xxmsy_tc[1,],TOTAL_B_APRX$xxmsy_tc[1,],
                TOTAL_B$xxmsy_med[1,],TOTAL_B_APRX$xxmsy_med[1,],
                TOTAL_B$xxmsy_w[1,],TOTAL_B_APRX$xxmsy_w[1,],
                bvb$dlm.geomean[1:n],bvbaprx$dlm.geomean[1:n])

Xex <- data.frame(TOTAL_U$xxmsy[1,],
                TOTAL_U$xxmsy_tc[1,],
                TOTAL_U$xxmsy_med[1,],
                TOTAL_U$xxmsy_w[1,],
                uvu$dlm.geomean[1:n])

col <- adjustcolor('black',alpha.f=0.2)

par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(2,2,2,2),xpd=FALSE,cex.axis=0.8)
  matplot(yrs,X,type='l',col=col,lty=1,ylim=c(0.8,2))
  mtext(side=2,expression('B/B'['MSY']),line=2.5)
  abline(h=1.0,lty=2)
  lines(yrs,rowMeans(X),lwd=2,col='red')
  lines(yrs,rowMeans(X)+2*apply(X,1,sd)/sqrt(10),lwd=1,lty=2,col='red')
  lines(yrs,rowMeans(X)-2*apply(X,1,sd)/sqrt(10),lwd=1,lty=2,col='red')

matplot(yrs,Xex,type='l',col=col,lty=1,ylim=c(0.8,2))
  mtext(side=2,expression('U/U'['MSY']),line=2.5)
  abline(h=1.0,lty=2)
  lines(yrs,rowMeans(Xex),lwd=2,col='red')
  lines(yrs,rowMeans(Xex)+2*apply(Xex,1,sd)/sqrt(5),lwd=1,lty=2,col='red')
  lines(yrs,rowMeans(Xex)-2*apply(Xex,1,sd)/sqrt(5),lwd=1,lty=2,col='red')
