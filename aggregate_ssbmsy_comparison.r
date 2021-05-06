##########################################################
## ONLY TOTAL BIOMASS REFERENCE POINTS ###################
##########################################################
matssbssbmsy <- matrix(NA,nrow=2020,ncol=length(stocks))
matssb       <- matrix(NA,nrow=2020,ncol=length(stocks))
matssbmsy    <- matrix(NA,nrow=2020,ncol=length(stocks))

for(i in 1:length(stocks)){
  d_tmp <- d[d$stockid==stocks[i],]
  SSB       <- d_tmp$SSB
  SSBSSBMSY <- d_tmp$SSBdivSSBmsy
  year      <- d_tmp$year
  
  if(sum(!is.na(SSB))>0){
    SSBMSY  <- mean(SSB/SSBSSBMSY,na.rm=TRUE)
    if(!is.finite(SSBMSY)){SSBMSY <- 0.5*max(SSB,na.rm=TRUE)}
    
    matssbssbmsy[year,i] <- SSB/SSBMSY
    matssb[year,i]       <- SSB
    matssbmsy[year,i]    <- SSBMSY
  }}

w <- colMeans(matssb,na.rm=TRUE)

ssbssbmsy=ssbssbmsy_w=ssbssbmsy_med=ssbssbmsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocks <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat     <- matssbssbmsy[yrs[i],]
  N           <- sum(!is.na(row_dat))
  nstocks[i]  <- N
  ssbssbmsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE), weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  ssbssbmsy[,i]      <- c(mean(row_dat,na.rm=TRUE), sd(row_dat,na.rm=TRUE)/sqrt(N))
  ssbssbmsy_med[,i]  <- c(median(row_dat,na.rm=TRUE), mad(row_dat,na.rm=TRUE)/sqrt(N))
  ssbssbmsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

TOTAL_SSB <- list(nstocks=nstocks,ssbssbmsy=ssbssbmsy,ssbssbmsy_w=ssbssbmsy_w,ssbssbmsy_med=ssbssbmsy_med,ssbssbmsy_tc=ssbssbmsy_tc)
TOTAL_SSB$currentSSBSSBMSY <- unlist(apply(matssbssbmsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

ddds <- numeric()
for(i in 1:length(stocks)){
  dd <- matssbssbmsy[,i]
  dd <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}

TOTAL_SSB$ssbssbmsy_trends <- ddds

ttt  <- apply(matssbssbmsy,2,function(x) sum(!is.na(x))>1)
tt   <- matssbssbmsy[,ttt]
ttry <- 1*apply(tt,2,function(x) x<1)

yes <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))
tot <- apply(ttry,1,function(x) sum(!is.na(x)))

TOTAL_SSB$proportion <- yes/tot


##########################################################
## APPROXIMATE ALL TBMSY = 0.5*max(TB) ###################
##########################################################
matssbssbmsy <- matrix(NA,nrow=2020,ncol=length(stocks))
matssb       <- matrix(NA,nrow=2020,ncol=length(stocks))
matssbmsy    <- matrix(NA,nrow=2020,ncol=length(stocks))

for(i in 1:length(stocks)){
  d_tmp <- d[d$stockid==stocks[i],]
  SSB   <- d_tmp$SSB
  year  <- d_tmp$year
  
  if(sum(!is.na(SSB)) > 0){
    SSBMSY <- 0.5*max(SSB,na.rm=TRUE)
    
    matssbssbmsy[year,i] <- SSB/SSBMSY
    matssb[year,i]       <- SSB
    matssbmsy[year,i]    <- SSBMSY
  }
}

w <- colMeans(matssb,na.rm=TRUE)

ssbssbmsy=ssbssbmsy_w=ssbssbmsy_med=ssbssbmsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocks <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat     <- matssbssbmsy[yrs[i],]
  N           <- sum(!is.na(row_dat))
  nstocks[i]  <- N
  ssbssbmsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE), weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  ssbssbmsy[,i]      <- c(mean(row_dat,na.rm=TRUE), sd(row_dat,na.rm=TRUE)/sqrt(N))
  ssbssbmsy_med[,i]  <- c(median(row_dat,na.rm=TRUE), mad(row_dat,na.rm=TRUE)/sqrt(N))
  ssbssbmsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

TOTAL_SSB_APRX <- list(nstocks=nstocks,ssbssbmsy=ssbssbmsy,ssbssbmsy_w=ssbssbmsy_w,ssbssbmsy_med=ssbssbmsy_med,ssbssbmsy_tc=ssbssbmsy_tc)

ddds <- numeric()
for(i in 1:length(stocks)){
  dd <- matssbssbmsy[,i]
  dd <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}

TOTAL_SSB_APRX$ssbssbmsy_trends <- ddds
TOTAL_SSB_APRX$currentSSBSSBMSY <- unlist(apply(matssbssbmsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

ttt  <- apply(matssbssbmsy,2,function(x) sum(!is.na(x))>1)
tt   <- matssbssbmsy[,ttt]
ttry <- 1*apply(tt,2,function(x) x<1)

yes <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))
tot <- apply(ttry,1,function(x) sum(!is.na(x)))

TOTAL_SSB_APRX$proportion <- yes/tot
