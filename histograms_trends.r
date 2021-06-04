###################################################################
## CALCULATE PROPORTIONS ##########################################
###################################################################
TOTAL_B$proportion[2016]        
TOTAL_U$proportion[2016]  
sum(TOTAL_B$xxmsy_trends<0)/length(TOTAL_B$xxmsy_trends)
sum(TOTAL_U$xxmsy_trends>0)/length(TOTAL_U$xxmsy_trends)

max(TOTAL_B$proportion,na.rm=TRUE)                             #maximum proportion of overfished stocks 
which(TOTAL_B$proportion==max(TOTAL_B$proportion,na.rm=TRUE))  #year when it occured  
1-min(TOTAL_U$proportion,na.rm=TRUE)                     #maximum proportion of over exploited  
which(TOTAL_U$proportion==min(TOTAL_U$proportion,na.rm=TRUE)) #year when it occurs

#######################################
## PLOT ###############################
#######################################
cols <- brewer.pal(6,'Dark2')
nyrs <- yrs[n] - 1999 #years between 1999 and 2016

slope <- function(y){
  fit <- lm(y[21:n] ~ seq(1,nyrs))
  return(summary(fit)$coefficients[2,1])
}
slope_w <- function(y,w){
  fit <- lm(y[21:n] ~ seq(1,nyrs),w=w[21:n])
  return(summary(fit)$coefficients[2,1])
}

status <- c(TOTAL_B$xxmsy[1,n],
            TOTAL_B$xxmsy_w[1,n],
            TOTAL_B$xxmsy_med[1,n],
            TOTAL_B$xxmsy_tc[1,n],
            bvb$dlm.geomean[n],
            TOTAL_B_APRX$xxmsy[1,n],
            TOTAL_B_APRX$xxmsy_w[1,n],
            TOTAL_B_APRX$xxmsy_med[1,n],
            TOTAL_B_APRX$xxmsy_tc[1,n],
            bvbaprx$dlm.geomean[n])

exstatus <- c(TOTAL_U$xxmsy[1,n],
              TOTAL_U$xxmsy_w[1,n],
              TOTAL_U$xxmsy_med[1,n],
              TOTAL_U$xxmsy_tc[1,n],
              uvu$dlm.geomean[n])

status_slope <- c(slope(TOTAL_B$xxmsy[1,]),
                  slope(TOTAL_B$xxmsy_w[1,]),
                  slope(TOTAL_B$xxmsy_med[1,]),
                  slope(TOTAL_B$xxmsy_tc[1,]),
                  slope(bvb$dlm.geomean),
                  slope(TOTAL_B_APRX$xxmsy[1,]),
                  slope(TOTAL_B_APRX$xxmsy_w[1,]),
                  slope(TOTAL_B_APRX$xxmsy_med[1,]),
                  slope(TOTAL_B_APRX$xxmsy_tc[1,]),
                  slope(bvbaprx$dlm.geomean))

exstatus_slope <- c(slope(TOTAL_U$xxmsy[1,]),
                  slope(TOTAL_U$xxmsy_w[1,]),
                  slope(TOTAL_U$xxmsy_med[1,]),
                  slope(TOTAL_U$xxmsy_tc[1,]),
                  slope(uvu$dlm.geomean))

#################################################
## TOTAL BIOMASS ################################
#################################################
par(mfrow=c(2,2),xpd=FALSE,mar=c(2,2,2,2),oma=c(2,2,2,2),cex.axis=0.8)
hist(TOTAL_B$currentXXMSY[TOTAL_B$currentXXMSY<4],breaks=25,xlim=c(0,2),main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[2],alpha.f=0.5),border=FALSE)
hist(TOTAL_B_APRX$currentXXMSY[TOTAL_B_APRX$currentXXMSY<2],breaks=25,xlim=c(0,2),main='',freq=FALSE,ylim=c(0,1.2),add=TRUE,col=adjustcolor(cols[1],alpha.f=0.25),border=FALSE)
  mtext(side=1,expression(italic('B/B'['MSY'])),line=2.5)
  mtext(side=2,expression('Frequency Density'),line=2.5)
    abline(v=1,lwd=1,lty=1,col='red')
    abline(v=mean(status))
    abline(v=mean(status)+2*c(-sd(status),sd(status))/sqrt(length(status)),lty=2)
    
hist(TOTAL_U$currentXXMSY[TOTAL_U$currentXXMSY<4],breaks=25,main='',freq=FALSE,ylim=c(0,1.2),border=FALSE)
  abline(v=1,lwd=1,lty=1,col='red')
  abline(v=mean(exstatus))
  abline(v=mean(exstatus)+2*c(-sd(exstatus),sd(exstatus))/sqrt(length(exstatus)),lty=2)
    mtext(side=1,expression(italic('U/U'['MSY'])),line=2.5)

hist(TOTAL_B$xxmsy_trends,breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE,xlim=c(-0.2,0.2))
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(status_slope))
  abline(v=mean(status_slope)+2*c(-sd(status_slope),sd(status_slope))/sqrt(length(exstatus)),lty=2)
    mtext(side=1,expression(italic('Trend in B/B'['MSY'])),line=2.5)
    mtext(side=2,'Frequency Density',line=2.5)

hist(TOTAL_U$xxmsy_trends[TOTAL_U$xxmsy_trends>-0.4 & TOTAL_U$xxmsy_trends<0.4],breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE)
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(exstatus_slope))
  abline(v=mean(exstatus_slope)+2*c(-sd(exstatus_slope),sd(exstatus_slope))/sqrt(length(exstatus_slope)),lty=2)
    mtext(side=1,expression(italic('Trend in U/U'['MSY'])),line=2.5)

#################################################
## SPAWNING STOCK BIOMASS #######################
#################################################

status_ssb <- c(TOTAL_SSB$xxmsy[1,n],
            TOTAL_SSB$xxmsy_w[1,n],
            TOTAL_SSB$xxmsy_med[1,n],
            TOTAL_SSB$xxmsy_tc[1,n],
            ssbvssb$dlm.geomean[n],
            TOTAL_SSB_APRX$xxmsy[1,n],
            TOTAL_SSB_APRX$xxmsy_w[1,n],
            TOTAL_SSB_APRX$xxmsy_med[1,n],
            TOTAL_SSB_APRX$xxmsy_tc[1,n],
            ssbvssbaprx$dlm.geomean[n])

status_slope_ssb <- c(slope(TOTAL_SSB$xxmsy[1,]),
                  slope(TOTAL_SSB$xxmsy_w[1,]),
                  slope(TOTAL_SSB$xxmsy_med[1,]),
                  slope(TOTAL_SSB$xxmsy_tc[1,]),
                  slope(ssbvssb$dlm.geomean),
                  slope(TOTAL_SSB_APRX$xxmsy[1,]),
                  slope(TOTAL_SSB_APRX$xxmsy_w[1,]),
                  slope(TOTAL_SSB_APRX$xxmsy_med[1,]),
                  slope(TOTAL_SSB_APRX$xxmsy_tc[1,]),
                  slope(ssbvssbaprx$dlm.geomean))

par(mfrow=c(1,2),xpd=FALSE,mar=c(2,2,2,2),oma=c(2,2,2,2),cex.axis=0.8)
hist(TOTAL_SSB$currentXXMSY[TOTAL_SSB$currentXXMSY<=2],xlim=c(0,2),breaks=25,main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[2],alpha.f=0.5),border=FALSE)
hist(TOTAL_SSB_APRX$currentXXMSY[TOTAL_SSB$currentXXMSY<=2],xlim=c(0,2),breaks=25,main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[1],alpha.f=0.5),border=FALSE,add=TRUE)
  mtext(side=1,expression(italic('SSB/SSB'['MSY'])),line=2.5)
  mtext(side=2,expression('Frequency Density'),line=2.5)
  abline(v=1,lwd=1,lty=1,col='red')
  abline(v=mean(status_ssb))
  abline(v=mean(status_ssb)+2*c(-sd(status_ssb),sd(status_ssb))/sqrt(length(status_ssb)),lty=2)

hist(TOTAL_SSB$xxmsy_trends,breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE,xlim=c(-0.2,0.2))
  mtext(side=1,expression(italic('Trend in SSB/SSB'['MSY'])),line=2.5)
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(status_slope_ssb))
  abline(v=mean(status_slope_ssb)+2*c(-sd(status_slope_ssb),sd(status_slope_ssb))/sqrt(length(status_slope_ssb)),lty=2)
  
