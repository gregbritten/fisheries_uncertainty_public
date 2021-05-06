##########################################################
## ONLY TOTAL BIOMASS REFERENCE POINTS ###################
##########################################################
matuumsy <- matrix(NA,nrow=2020,ncol=length(stocks))
matb     <- matrix(NA,nrow=2020,ncol=length(stocks))

for(i in 1:length(stocks)){
	d_tmp <- d[d$stockid==stocks[i],]
	TB      <- d_tmp$TB
	UUMSY   <- d_tmp$UdivUmsypref
	year    <- d_tmp$year
	
if(sum(!is.na(TB))>0 & sum(!is.na(UUMSY))>0){	
	B <- TB
				
	matuumsy[year,i] <- UUMSY
 	matb[year,i]     <- B
}}
			
w <- colMeans(matb,na.rm=TRUE)

uumsy=uumsy_w=uumsy_med=uumsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocksu <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat      <- matuumsy[yrs[i],]
  N            <- sum(!is.na(row_dat))
  nstocksu[i]  <- N
  uumsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE), weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  uumsy[,i]      <- c(mean(row_dat,na.rm=TRUE), sd(row_dat,na.rm=TRUE)/sqrt(N))
  uumsy_med[,i]  <- c(median(row_dat,na.rm=TRUE), mad(row_dat,na.rm=TRUE)/sqrt(N))
  uumsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

TOTAL_B$nstocksu  <- nstocksu
TOTAL_B$uumsy_w   <- uumsy_w
TOTAL_B$uumsy     <- uumsy
TOTAL_B$uumsy_med <- uumsy_med
TOTAL_B$uumsy_tc  <- uumsy_tc

TOTAL_B$currentUUMSY <- unlist(apply(matuumsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

ddds <- numeric()
for(i in 1:length(stocks)){
  dd <- matuumsy[,i]
  dd <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}

TOTAL_B$uumsy_trends <- ddds

ttt  <- apply(matuumsy,2,function(x) sum(!is.na(x))>1)
tt   <- matuumsy[,ttt]
ttry <- 1*apply(tt,2,function(x) x<1)

yes <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))
tot <- apply(ttry,1,function(x) sum(!is.na(x)))

TOTAL_B$uumsy_proportion <- yes/tot



