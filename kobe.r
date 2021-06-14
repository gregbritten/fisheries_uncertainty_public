rm(list=ls())
library(RColorBrewer)
library(Cairo)

##--SET YOUR WORKING DIRECTORY--###################
setwd('~/dropbox/working/fisheries_uncertainty/github_public')

##--FUNCTIONS--####################################
source('aggregate_function.r')
source('other_functions.r')

##--DATA--#########################################
d      <- read.csv('data/RAMLDB v4.491 (assessment data only)_timeseries_values_views.csv',stringsAsFactors=FALSE)
stocks <- unique(d$stockid)
yrs    <- 1980:2016
n      <- length(yrs)

##--MEAN TOTAL CATCH WEIGHTS--#######################
tc <- rep(NA,length(stocks))
for(i in 1:length(stocks)){
  d_tmp <- d[d$stockid==stocks[i],]
  TC    <- d_tmp$TCbest
  tc[i] <- mean(TC,na.rm=TRUE)
}; tc[tc=='NaN']=NA

##--AGGREGATIONS--#################################
TOTAL_B        <- aggregate_index(x='TB', approx_all=FALSE)
TOTAL_B_APRX   <- aggregate_index(x='TB', approx_all=TRUE)
TOTAL_SSB      <- aggregate_index(x='SSB',approx_all=FALSE)
TOTAL_SSB_APRX <- aggregate_index(x='SSB',approx_all=TRUE)
TOTAL_U        <- aggregate_index(x='U')

BvB         <- read.csv('data/BvBstocks.csv');      bvb         <- BvB[BvB$year%in%yrs,]
BvBaprx     <- read.csv('data/BvBstocks_aprx.csv'); bvbaprx     <- BvBaprx[BvBaprx$year%in%yrs,]
UvU         <- read.csv('data/UvUstocks.csv');      uvu         <- UvU[UvU$year%in%yrs,]
SSBvSSB     <- read.csv('data/SSBvSSB.csv');        ssbvssb     <- SSBvSSB[SSBvSSB$year%in%yrs,]
SSBvSSBaprx <- read.csv('data/SSBvSSB_aprx.csv');   ssbvssbaprx <- SSBvSSBaprx[SSBvSSBaprx$year%in%yrs,]

###########################################################
## KOBE PLOTS #############################################
###########################################################
alpha  <- 0.7
lincol <- 'grey'

par(mfrow=c(2,5),mar=c(2,1,4,1),oma=c(4,4,3,3),xpd=TRUE)
	plot(TOTAL_U$xxmsy[1,],TOTAL_B$xxmsy[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,bty='n',type='n',xlab='',ylab='')
		rects(alpha=alpha); segg()
		lines(TOTAL_B$xxmsy[1,],TOTAL_U$xxmsy[1,],col=lincol)
		segs(TOTAL_B$xxmsy[1,],TOTAL_B$xxmsy[2,],TOTAL_U$xxmsy[1,],TOTAL_U$xxmsy[2,])
		mtext('Unweighted Mean',adj=1,cex=1); mtext('a',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy_w[1,],TOTAL_B$xxmsy_w[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); 	segg()
		lines(TOTAL_B$xxmsy_w[1,],TOTAL_U$xxmsy_w[1,],col=lincol)
		segs(TOTAL_B$xxmsy_w[1,],TOTAL_B$xxmsy_w[2,],TOTAL_U$xxmsy_w[1,],TOTAL_U$xxmsy_w[2,])
		mtext('Weighted Mean',adj=1,cex=1); mtext('b',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy_med[1,],TOTAL_U$xxmsy_med[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); 		segg()
		lines(TOTAL_B$xxmsy_med[1,],TOTAL_U$xxmsy_med[1,],col=lincol)
		segs(TOTAL_B$xxmsy_med[1,],TOTAL_B$xxmsy_med[2,],TOTAL_U$xxmsy_med[1,],TOTAL_U$xxmsy_med[2,])
		mtext('Median',adj=1,cex=1); mtext('c',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy_tc[1,],TOTAL_B$xxmsy_tc[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); 		segg()
		lines(TOTAL_B$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[1,],col=lincol)
		segs(TOTAL_B$xxmsy_tc[1,],TOTAL_B$xxmsy_tc[2,],TOTAL_U$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[2,])
		mtext('Mean Catch',adj=1,cex=1); mtext('d',adj=0.05,line=0)
	plot(uvu$dlm.geomean,bvb$dlm.geomean[1:n],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); 		segg()
		lines(bvb$dlm.geomean,uvu$dlm.geomean,col=lincol)
		segs(bvb$dlm.geomean,bvb$dlm.geomean-bvb$dlm.lower,uvu$dlm.geomean,uvu$dlm.geomean-uvu$dlm.lower)
		mtext('State Space Model',adj=1,cex=1); mtext('e',adj=0.05,line=0)

		
	plot(TOTAL_U$xxmsy[1,],TOTAL_B_APRX$xxmsy[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,bty='n',type='n',xlab='',ylab='')
		rects(alpha=alpha); segg()
		lines(TOTAL_B_APRX$xxmsy[1,],TOTAL_U$xxmsy[1,],col='dark grey')
		segs(TOTAL_B_APRX$xxmsy[1,],TOTAL_B_APRX$xxmsy[2,],TOTAL_U$xxmsy[1,],TOTAL_U$xxmsy[2,])
		mtext('Unweighted Mean',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('f',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy[1,],TOTAL_B_APRX$xxmsy_w[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); segg()
		lines(TOTAL_B_APRX$xxmsy_w[1,],TOTAL_U$xxmsy_w[1,],col='dark grey')
		segs(TOTAL_B_APRX$xxmsy_w[1,],TOTAL_B_APRX$xxmsy_w[2,],TOTAL_U$xxmsy_w[1,],TOTAL_U$xxmsy_w[2,])
		mtext('Weighted Mean',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('g',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy_med[1,],TOTAL_B_APRX$xxmsy_med[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); segg()
		lines(TOTAL_B_APRX$xxmsy_med[1,],TOTAL_U$xxmsy_med[1,],col='dark grey')
		segs(TOTAL_B_APRX$xxmsy_med[1,],TOTAL_B_APRX$xxmsy_med[2,],TOTAL_U$xxmsy_med[1,],TOTAL_U$xxmsy_med[2,])
		mtext('Median',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('h',adj=0.05,line=0)
	plot(TOTAL_U$xxmsy_tc[1,],TOTAL_B_APRX$xxmsy_tc[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha); 		segg()
		lines(TOTAL_B_APRX$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[1,],col='dark grey')
		segs(TOTAL_B_APRX$xxmsy_tc[1,],TOTAL_B_APRX$xxmsy_tc[2,],TOTAL_U$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[2,])
		mtext('Mean Catch',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('i',adj=0.05,line=0)
	plot(uvu$dlm.geomean,bvbaprx$dlm.geomean[1:n],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
		rects(alpha=alpha)
		lines(bvbaprx$dlm.geomean[1:n],uvu$dlm.geomean,col='dark grey')
		segg()
		segs(bvbaprx$dlm.geomean,bvbaprx$dlm.geomean-bvbaprx$dlm.lower,uvu$dlm.geomean,uvu$dlm.geomean-uvu$dlm.lower)
		mtext('State Space Model',adj=1,cex=1,line=1);mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); 		mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('j',adj=0.05,line=0)

		mtext(side=2,outer=TRUE,expression('U/U'['MSY']),cex=1.3,line=1.5)
		mtext(side=1,outer=TRUE,expression('B/B'['MSY']),cex=1.3,line=1.5)

##################################################################
## SUPPLEMENTARY KOBE PLOT #######################################
##################################################################
par(mfrow=c(2,5),mar=c(2,1,4,1),oma=c(4,4,3,3),xpd=TRUE)
  plot(TOTAL_U$xxmsy[1,],TOTAL_SSB$xxmsy[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,bty='n',type='n',xlab='',ylab='')
    rects(alpha=alpha); segg()
    lines(TOTAL_SSB$xxmsy[1,],TOTAL_U$xxmsy[1,],col=lincol)
    segs(TOTAL_SSB$xxmsy[1,],TOTAL_SSB$xxmsy[2,],TOTAL_U$xxmsy[1,],TOTAL_U$xxmsy[2,])
    mtext('Unweighted Mean',adj=1,cex=1); mtext('a',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy_w[1,],TOTAL_SSB$xxmsy_w[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); 	segg()
    lines(TOTAL_SSB$xxmsy_w[1,],TOTAL_U$xxmsy_w[1,],col=lincol)
    segs(TOTAL_SSB$xxmsy_w[1,],TOTAL_SSB$xxmsy_w[2,],TOTAL_U$xxmsy_w[1,],TOTAL_U$xxmsy_w[2,])
    mtext('Weighted Mean',adj=1,cex=1); mtext('b',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy_med[1,],TOTAL_SSB$xxmsy_med[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); 		segg()
    lines(TOTAL_SSB$xxmsy_med[1,],TOTAL_U$xxmsy_med[1,],col=lincol)
    segs(TOTAL_SSB$xxmsy_med[1,],TOTAL_SSB$xxmsy_med[2,],TOTAL_U$xxmsy_med[1,],TOTAL_U$xxmsy_med[2,])
    mtext('Median',adj=1,cex=1); mtext('c',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy_tc[1,],TOTAL_SSB$xxmsy_tc[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); 		segg()
    lines(TOTAL_SSB$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[1,],col=lincol)
    segs(TOTAL_SSB$xxmsy_tc[1,],TOTAL_SSB$xxmsy_tc[2,],TOTAL_U$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[2,])
    mtext('Mean Catch',adj=1,cex=1); mtext('d',adj=0.05,line=0)
  plot(uvu$dlm.geomean,ssbvssb$dlm.geomean,ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); 		segg()
    lines(ssbvssb$dlm.geomean[1:n],uvu$dlm.geomean,col=lincol)
    segs(ssbvssb$dlm.geomean,ssbvssb$dlm.geomean-ssbvssb$dlm.lower,uvu$dlm.geomean,uvu$dlm.geomean-uvu$dlm.lower)
    mtext('State Space Model',adj=1,cex=1); mtext('e',adj=0.05,line=0)
  
  
  plot(TOTAL_U$xxmsy[1,],TOTAL_SSB_APRX$xxmsy[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,bty='n',type='n',xlab='',ylab='')
    rects(alpha=alpha); segg()
    lines(TOTAL_SSB_APRX$xxmsy[1,],TOTAL_U$xxmsy[1,],col='dark grey')
    segs(TOTAL_SSB_APRX$xxmsy[1,],TOTAL_SSB_APRX$xxmsy[2,],TOTAL_U$xxmsy[1,],TOTAL_U$xxmsy[2,])
    mtext('Unweighted Mean',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('f',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy[1,],TOTAL_SSB_APRX$xxmsy_w[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); segg()
    lines(TOTAL_SSB_APRX$xxmsy_w[1,],TOTAL_U$xxmsy_w[1,],col='dark grey')
    segs(TOTAL_SSB_APRX$xxmsy_w[1,],TOTAL_SSB_APRX$xxmsy_w[2,],TOTAL_U$xxmsy_w[1,],TOTAL_U$xxmsy_w[2,])
    mtext('Weighted Mean',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('g',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy_med[1,],TOTAL_SSB_APRX$xxmsy_med[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); segg()
    lines(TOTAL_SSB_APRX$xxmsy_med[1,],TOTAL_U$xxmsy_med[1,],col='dark grey')
    segs(TOTAL_SSB_APRX$xxmsy_med[1,],TOTAL_SSB_APRX$xxmsy_med[2,],TOTAL_U$xxmsy_med[1,],TOTAL_U$xxmsy_med[2,])
    mtext('Median',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('h',adj=0.05,line=0)
  plot(TOTAL_U$xxmsy_tc[1,],TOTAL_SSB_APRX$xxmsy_tc[1,],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha); 		segg()
    lines(TOTAL_SSB_APRX$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[1,],col='dark grey')
    segs(TOTAL_SSB_APRX$xxmsy_tc[1,],TOTAL_SSB_APRX$xxmsy_tc[2,],TOTAL_U$xxmsy_tc[1,],TOTAL_U$xxmsy_tc[2,])
    mtext('Mean Catch',adj=1,cex=1,line=1); mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('i',adj=0.05,line=0)
  plot(uvu$dlm.geomean,ssbvssbaprx$dlm.geomean[1:n],ylim=c(0,2),xlim=c(0,2),cex=0.1,type='n',bty='n',xlab='',ylab='')
    rects(alpha=alpha)
    lines(ssbvssbaprx$dlm.geomean[1:n],uvu$dlm.geomean,col='dark grey')
    segg()
    segs(ssbvssbaprx$dlm.geomean,ssbvssbaprx$dlm.geomean-ssbvssbaprx$dlm.lower,uvu$dlm.geomean,uvu$dlm.geomean-uvu$dlm.lower)
    mtext('State Space Model',adj=1,cex=1,line=1);mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); 		mtext('Historical Biomass Target',adj=1,cex=0.6,line=0); mtext('j',adj=0.05,line=0)
  
  mtext(side=2,outer=TRUE,expression('U/U'['MSY']),cex=1.3,line=1.5)
  mtext(side=1,outer=TRUE,expression('SSB/SSB'['MSY']),cex=1.3,line=1.5)


