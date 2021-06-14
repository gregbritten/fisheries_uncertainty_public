##--FUNCTIONS FOR WEIGHTED STATISTICS--##############################
weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}

weighted.sd <- function(x, w, na.rm = TRUE) sqrt(weighted.var(x, w, na.rm = TRUE))

weighted_mean <- function(x, w, ..., na.rm = FALSE){
    if(na.rm){
    df_omit <- na.omit(data.frame(x, w))
      return(weighted.mean(df_omit$x, df_omit$w, ...))
    } 
  weighted.mean(x, w, ...)
}

##--PLOTTING FUNCTIONS--##############################
rects <- function(alpha){
  rect(xleft=0,xright=1,ybottom=1,ytop=2,col=adjustcolor('red',alpha.f=alpha),border=NA)
  rect(xleft=1,xright=2,ybottom=1,ytop=2,col=adjustcolor('yellow',alpha.f=alpha),border=NA)
  rect(xleft=0,xright=1,ybottom=0,ytop=1,col=adjustcolor('yellow',alpha.f=alpha),border=NA)
  rect(xleft=1,xright=2,ybottom=0,ytop=1,col=adjustcolor('green',alpha.f=alpha),border=NA)
}

CIs <- function(muX,sdX,muY,sdY){
  lines(x=muX-sdX,y=muY-sdY,lty=2)
  lines(x=muX+sdX,y=muY+sdY,lty=2)
}

segs <- function(muX,sdX,muY,sdY){
  points(muX[1],muY[1],pch=8)
  segments(x0=muX[n]-2*sdX[n],x1=muX[n]+2*sdX[n],y0=muY[n],         y1=muY[n],lty=1,lwd=2)
  segments(x0=muX[n],      x1=muX[n],         y0=muY[n]+2*sdY[n],y1=muY[n]-2*sdY[n],lty=1,lwd=2)
}

segg <- function(){
  segments(x0=0,x1=2,y0=1,y1=1,lty=2)
  segments(x0=1,x1=1,y0=0,y1=2,lty=2)
}

labs <- function(){
  mtext(side=2,line=2.5,expression('U/U'['MSY']))
  mtext(side=1,line=2.5,expression('B/B'['MSY']))
}

