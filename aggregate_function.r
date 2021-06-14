# x is the variable for anlaysis (TB, SSB, or U)
# approx_all is TRUE/FALSE to determine if we approximate all stocks by 0.5max(B)
# dataset 'd', stock list 'stocks', total catch vector 'tc' are loaded outside
aggregate_index <- function(x,approx_all){

matb=matbmsy=matxxmsy <- matrix(NA,nrow=2020,ncol=length(stocks)) 


for(i in 1:length(stocks)){
  d_tmp <- d[d$stockid==stocks[i],]
  year    <- d_tmp$year
  
  if(x=='TB'|x=='SSB'){
  if(x=='TB'){
  	B       <- d_tmp$TB
  	BBMSY   <- d_tmp$TBdivTBmsy
  }else{
  if(x=='SSB'){
    B       <- d_tmp$SSB
    BBMSY   <- d_tmp$SSBdivSSBmsy
  }}
    
	if(sum(!is.na(B))>0){ 
	  if(approx_all==FALSE){
    	BMSY  <- mean(B/BBMSY,na.rm=TRUE)  
	    if(!is.finite(BMSY)){BMSY <- 0.5*max(B,na.rm=TRUE)}
	  }
	  if(approx_all==TRUE){BMSY <- 0.5*max(B,na.rm=TRUE)}

		matxxmsy[year,i] <- B/BMSY
		matb[year,i]     <- B
		matbmsy[year,i]  <- BMSY
	 }
  }else{
    if(x=='U'){
      B      <- d_tmp$TB
      UUMSY  <- d_tmp$UdivUmsypref
  
      if(sum(!is.na(B))>0 & sum(!is.na(UUMSY))>0){	
        matxxmsy[year,i] <- UUMSY
        matb[year,i]     <- B
      }
    }
  }
}

w <- colMeans(matb,na.rm=TRUE)

xxmsy=xxmsy_w=xxmsy_med=xxmsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocks <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat     <- matxxmsy[yrs[i],]
  N           <- sum(!is.na(row_dat))
  nstocks[i]  <- N
  xxmsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE),  weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  xxmsy[,i]      <- c(mean(row_dat,na.rm=TRUE),               sd(row_dat,na.rm=TRUE)/sqrt(N))
  xxmsy_med[,i]  <- c(median(row_dat,na.rm=TRUE),             mad(row_dat,na.rm=TRUE)/sqrt(N))
  xxmsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

OUT              <- list(nstocks=nstocks,xxmsy=xxmsy,xxmsy_w=xxmsy_w,xxmsy_med=xxmsy_med,xxmsy_tc=xxmsy_tc)
OUT$currentXXMSY <- unlist(apply(matxxmsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

##############################################################
##--Compute trend over last ten years--#######################
##############################################################
ddds <- numeric()
for(i in 1:length(stocks)){
  dd  <- matxxmsy[,i]
  dd  <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>=10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}
OUT$xxmsy_trends <- ddds

##############################################################
##--Count stocks above and below--############################
##############################################################
ttt  <- apply(matxxmsy,2,function(x) sum(!is.na(x))>1) 
tt   <- matxxmsy[,ttt]                                 
ttry <- 1*apply(tt,2,function(x) x<1)                  

yes  <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))    
tot  <- apply(ttry,1,function(x) sum(!is.na(x)))       

OUT$proportion <- yes/tot

  return(OUT)

}
