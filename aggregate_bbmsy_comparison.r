##########################################################
## ONLY TOTAL BIOMASS REFERENCE POINTS ###################
##########################################################
matbbmsy <- matrix(NA,nrow=2020,ncol=length(stocks)) #row number is year, columns for all possible stocks
matb     <- matrix(NA,nrow=2020,ncol=length(stocks))
matbmsy  <- matrix(NA,nrow=2020,ncol=length(stocks))

for(i in 1:length(stocks)){
	d_tmp <- d[d$stockid==stocks[i],]
	TB        <- d_tmp$TB
	TBTBMSY   <- d_tmp$TBdivTBmsy
	year      <- d_tmp$year

	if(sum(!is.na(TB))>0){                   #check if there are any non-NA data points
		TBMSY  <- mean(TB/TBTBMSY,na.rm=TRUE)  #extract implied TBMSY from ratio 
		if(!is.finite(TBMSY)){TBMSY <- 0.5*max(TB,na.rm=TRUE)}                  #check if we have TBMSY

		matbbmsy[year,i] <- TB/TBMSY
		matb[year,i]     <- TB
		matbmsy[year,i]  <- TBMSY
}}

w <- colMeans(matb,na.rm=TRUE)

bbmsy=bbmsy_w=bbmsy_med=bbmsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocks <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat     <- matbbmsy[yrs[i],]
  N           <- sum(!is.na(row_dat))
  nstocks[i]  <- N
  bbmsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE), weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  bbmsy[,i]      <- c(mean(row_dat,na.rm=TRUE), sd(row_dat,na.rm=TRUE)/sqrt(N))
  bbmsy_med[,i]  <- c(median(row_dat,na.rm=TRUE), mad(row_dat,na.rm=TRUE)/sqrt(N))
  bbmsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

TOTAL_B              <- list(nstocks=nstocks,bbmsy=bbmsy,bbmsy_w=bbmsy_w,bbmsy_med=bbmsy_med,bbmsy_tc=bbmsy_tc)
TOTAL_B$currentBBMSY <- unlist(apply(matbbmsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

##--Compute trend over last ten years--#######################
ddds <- numeric()
for(i in 1:length(stocks)){
  dd <- matbbmsy[,i]
  dd <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}
TOTAL_B$bbmsy_trends <- ddds  #attach to list

ttt  <- apply(matbbmsy,2,function(x) sum(!is.na(x))>1) #check columns for data
tt   <- matbbmsy[,ttt]                                 #submit where there are data
ttry <- 1*apply(tt,2,function(x) x<1)                  #indicator where B/Bmsy < 1

yes  <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))    #count where B/Bmsy<1
tot <- apply(ttry,1,function(x) sum(!is.na(x)))        #find deonominator as number of non NAs

TOTAL_B$proportion <- yes/tot
##########################################################
## APPROXIMATE ALL TBMSY = 0.5*max(TB) ###################
##########################################################
matbbmsy <- matrix(NA,nrow=2020,ncol=length(stocks))
matb     <- matrix(NA,nrow=2020,ncol=length(stocks))
matbmsy  <- matrix(NA,nrow=2020,ncol=length(stocks))

for(i in 1:length(stocks)){
	d_tmp <- d[d$stockid==stocks[i],]
	TB        <- d_tmp$TB
	year      <- d_tmp$year
	
	if(sum(!is.na(TB)) > 0){
		TBMSY <- 0.5*max(TB,na.rm=TRUE)
				
	matbbmsy[year,i] <- TB/TBMSY
 	matb[year,i]     <- TB
	matbmsy[year,i]  <- TBMSY
}
}

w <- colMeans(matb,na.rm=TRUE)

bbmsy=bbmsy_w=bbmsy_med=bbmsy_tc=matrix(NA,nrow=2,ncol=length(yrs))
nstocks <- rep(NA,length(yrs))

for(i in 1:length(yrs)){
  row_dat     <- matbbmsy[yrs[i],]
  N           <- sum(!is.na(row_dat))
  nstocks[i]  <- N
  bbmsy_w[,i]    <- c(weighted_mean(row_dat,w=w,na.rm=TRUE), weighted.sd(row_dat,w=w,na.rm=TRUE)/sqrt(N))
  bbmsy[,i]      <- c(mean(row_dat,na.rm=TRUE), sd(row_dat,na.rm=TRUE)/sqrt(N))
  bbmsy_med[,i]  <- c(median(row_dat,na.rm=TRUE), mad(row_dat,na.rm=TRUE)/sqrt(N))
  bbmsy_tc[,i]   <- c(weighted_mean(row_dat,w=tc,na.rm=TRUE), weighted.sd(row_dat,w=tc,na.rm=TRUE)/sqrt(N))
}

TOTAL_B_APRX              <- list(nstocks=nstocks,bbmsy=bbmsy,bbmsy_w=bbmsy_w,bbmsy_med=bbmsy_med,bbmsy_tc=bbmsy_tc)
TOTAL_B_APRX$currentBBMSY <- unlist(apply(matbbmsy, 2, function(x){xx=x[!is.na(x)]; n=length(xx); return(xx[n])}))

ddds <- numeric()
for(i in 1:length(stocks)){
  dd <- matbbmsy[,i]
  dd <- dd[!is.na(dd)]
  ddd <- diff(dd)
  if(length(ddd)>10){
    ddds <- c(ddds,mean(ddd[(length(ddd)-10):length(ddd)]))
  }
}

TOTAL_B_APRX$bbmsy_trends <- ddds

ttt  <- apply(matbbmsy,2,function(x) sum(!is.na(x))>1)
tt   <- matbbmsy[,ttt]
ttry <- 1*apply(tt,2,function(x) x<1)

yes <- apply(ttry,1,function(x) sum(x,na.rm=TRUE))
tot <- apply(ttry,1,function(x) sum(!is.na(x)))

TOTAL_B_APRX$proportion <- yes/tot




