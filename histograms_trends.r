######################################
## CALCULATE PROPORTIONS #############
######################################
#sum(TOTAL_B$currentBBMSY<1)/length(TOTAL_B$currentBBMSY) #according to the most recent number
#sum(TOTAL_B$currentUUMSY>1)/length(TOTAL_B$currentUUMSY) 
TOTAL_B$proportion[2016]        
TOTAL_B$uumsy_proportion[2016]  
sum(TOTAL_B$bbmsy_trends<0)/length(TOTAL_B$bbmsy_trends)
sum(TOTAL_B$uumsy_trends>0)/length(TOTAL_B$uumsy_trends)

max(TOTAL_B$proportion,na.rm=TRUE)                             #maximum proportion of overfished stocks 
which(TOTAL_B$proportion==max(TOTAL_B$proportion,na.rm=TRUE))  #year when it occured  
1-min(TOTAL_B$uumsy_proportion,na.rm=TRUE)                     #maximum proportion of over exploited  
which(TOTAL_B$uumsy_proportion==min(TOTAL_B$uumsy_proportion,na.rm=TRUE)) #year when it occurs

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

status <- c(TOTAL_B$bbmsy[1,n],
            TOTAL_B$bbmsy_w[1,n],
            TOTAL_B$bbmsy_med[1,n],
            TOTAL_B$bbmsy_tc[1,n],
            bvb$dlm.geomean[n],
            TOTAL_B_APRX$bbmsy[1,n],
            TOTAL_B_APRX$bbmsy_w[1,n],
            TOTAL_B_APRX$bbmsy_med[1,n],
            TOTAL_B_APRX$bbmsy_tc[1,n],
            bvbaprx$dlm.geomean[n])

exstatus <- c(TOTAL_B$uumsy[1,n],
              TOTAL_B$uumsy_w[1,n],
              TOTAL_B$uumsy_med[1,n],
              TOTAL_B$uumsy_tc[1,n],
              uvu$dlm.geomean[n])

status_slope <- c(slope(TOTAL_B$bbmsy[1,]),
                  slope(TOTAL_B$bbmsy_w[1,]),
                  slope(TOTAL_B$bbmsy_med[1,]),
                  slope(TOTAL_B$bbmsy_tc[1,]),
                  slope(bvb$dlm.geomean),
                  slope(TOTAL_B_APRX$bbmsy[1,]),
                  slope(TOTAL_B_APRX$bbmsy_w[1,]),
                  slope(TOTAL_B_APRX$bbmsy_med[1,]),
                  slope(TOTAL_B_APRX$bbmsy_tc[1,]),
                  slope(bvbaprx$dlm.geomean))

exstatus_slope <- c(slope(TOTAL_B$uumsy[1,]),
                  slope(TOTAL_B$uumsy_w[1,]),
                  slope(TOTAL_B$uumsy_med[1,]),
                  slope(TOTAL_B$uumsy_tc[1,]),
                  slope(uvu$dlm.geomean))

#################################################
## TOTAL BIOMASS ################################
#################################################
par(mfrow=c(2,2),xpd=FALSE,mar=c(2,2,2,2),oma=c(2,2,2,2),cex.axis=0.8)
hist(TOTAL_B$currentBBMSY[TOTAL_B$currentBBMSY<4],breaks=25,xlim=c(0,2),main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[2],alpha.f=0.5),border=FALSE)
hist(TOTAL_B_APRX$currentBBMSY[TOTAL_B_APRX$currentBBMSY<2],breaks=25,xlim=c(0,2),main='',freq=FALSE,ylim=c(0,1.2),add=TRUE,col=adjustcolor(cols[1],alpha.f=0.25),border=FALSE)
  mtext(side=1,expression(italic('B/B'['MSY'])),line=2.5)
  mtext(side=2,expression('Frequency Density'),line=2.5)
    abline(v=1,lwd=1,lty=1,col='red')
    abline(v=mean(status))
    abline(v=mean(status)+2*c(-sd(status),sd(status))/sqrt(length(status)),lty=2)
    
hist(TOTAL_B$currentUUMSY[TOTAL_B$currentUUMSY<4],breaks=25,main='',freq=FALSE,ylim=c(0,1.2),border=FALSE)
  abline(v=1,lwd=1,lty=1,col='red')
  abline(v=mean(exstatus))
  abline(v=mean(exstatus)+2*c(-sd(exstatus),sd(exstatus))/sqrt(length(exstatus)),lty=2)
    mtext(side=1,expression(italic('U/U'['MSY'])),line=2.5)

hist(TOTAL_B$bbmsy_trends,breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE,xlim=c(-0.2,0.2))
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(status_slope))
  abline(v=mean(status_slope)+2*c(-sd(status_slope),sd(status_slope))/sqrt(length(exstatus)),lty=2)
    mtext(side=1,expression(italic('Trend in B/B'['MSY'])),line=2.5)
    mtext(side=2,'Frequency Density',line=2.5)

hist(TOTAL_B$uumsy_trends[TOTAL_B$uumsy_trends>-0.4 & TOTAL_B$uumsy_trends<0.4],breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE)
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(exstatus_slope))
  abline(v=mean(exstatus_slope)+2*c(-sd(exstatus_slope),sd(exstatus_slope))/sqrt(length(exstatus_slope)),lty=2)
    mtext(side=1,expression(italic('Trend in U/U'['MSY'])),line=2.5)

#################################################
## SPAWNING STOCK BIOMASS #######################
#################################################

status_ssb <- c(TOTAL_SSB$ssbssbmsy[1,n],
            TOTAL_SSB$ssbssbmsy_w[1,n],
            TOTAL_SSB$ssbssbmsy_med[1,n],
            TOTAL_SSB$ssbssbmsy_tc[1,n],
            ssbvssb$dlm.geomean[n],
            TOTAL_SSB_APRX$ssbssbmsy[1,n],
            TOTAL_SSB_APRX$ssbssbmsy_w[1,n],
            TOTAL_SSB_APRX$ssbssbmsy_med[1,n],
            TOTAL_SSB_APRX$ssbssbmsy_tc[1,n],
            ssbvssbaprx$dlm.geomean[n])

status_slope_ssb <- c(slope(TOTAL_SSB$ssbssbmsy[1,]),
                  slope(TOTAL_SSB$ssbssbmsy_w[1,]),
                  slope(TOTAL_SSB$ssbssbmsy_med[1,]),
                  slope(TOTAL_SSB$ssbssbmsy_tc[1,]),
                  slope(ssbvssb$dlm.geomean),
                  slope(TOTAL_SSB_APRX$ssbssbmsy[1,]),
                  slope(TOTAL_SSB_APRX$ssbssbmsy_w[1,]),
                  slope(TOTAL_SSB_APRX$ssbssbmsy_med[1,]),
                  slope(TOTAL_SSB_APRX$ssbssbmsy_tc[1,]),
                  slope(ssbvssbaprx$dlm.geomean))

par(mfrow=c(1,2),xpd=FALSE,mar=c(2,2,2,2),oma=c(2,2,2,2),cex.axis=0.8)
hist(TOTAL_SSB$currentSSBSSBMSY[TOTAL_SSB$currentSSBSSBMSY<=2],xlim=c(0,2),breaks=25,main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[2],alpha.f=0.5),border=FALSE)
hist(TOTAL_SSB_APRX$currentSSBSSBMSY[TOTAL_SSB$currentSSBSSBMSY<=2],xlim=c(0,2),breaks=25,main='',freq=FALSE,ylim=c(0,1.2),col=adjustcolor(cols[1],alpha.f=0.5),border=FALSE,add=TRUE)
  mtext(side=1,expression(italic('SSB/SSB'['MSY'])),line=2.5)
  mtext(side=2,expression('Frequency Density'),line=2.5)
  abline(v=1,lwd=1,lty=1,col='red')
  abline(v=mean(status_ssb))
  abline(v=mean(status_ssb)+2*c(-sd(status_ssb),sd(status_ssb))/sqrt(length(status_ssb)),lty=2)

hist(TOTAL_SSB$ssbssbmsy_trends,breaks=25,main='',freq=FALSE,ylim=c(0,15),border=FALSE,xlim=c(-0.2,0.2))
  mtext(side=1,expression(italic('Trend in SSB/SSB'['MSY'])),line=2.5)
  abline(v=0,lwd=1,lty=1,col='red')
  abline(v=mean(status_slope_ssb))
  abline(v=mean(status_slope_ssb)+2*c(-sd(status_slope_ssb),sd(status_slope_ssb))/sqrt(length(status_slope_ssb)),lty=2)
  
