
par(mfrow=c(1,1))
plot(yrs,TOTAL_B$proportion[yrs],type='l',ylim=c(0.1,0.8),xlab='',ylab='')
  lines(yrs,TOTAL_B_APRX$proportion[yrs],type='l',lty=2)
  lines(yrs,TOTAL_U$proportion[yrs],type='l',col='red')
  abline(h=0.5,lty=1)
  legend('bottomright',legend=c(expression(italic('p(B<B'['MSY']*')')),
                              expression(italic('p(B<B'['MSY']*' historical)')),
                              expression(italic('p(SSB<SSB'['MSY']*')')),
                              expression(italic('p(SSB<SSB'['MSY']*' historical)')),
                              expression(italic('p(U<U'['MSY']*')'))),bty='n',
        lty=c(1,2,1,2,1),col=c('black','black','blue','blue','red'))
  mtext(side=2,'Proportion of Stocks',line=2.5)

plot(1950:yrs[n],TOTAL_B$proportion[1950:yrs[n]],type='l',ylim=c(0.1,1.0),xlab='',ylab='')
  lines(1950:yrs[n],TOTAL_B_APRX$proportion[1950:yrs[n]],type='l',lty=2)
  lines(1950:yrs[n],TOTAL_U$proportion[1950:yrs[n]],type='l',col='red')
  abline(h=0.5,lty=1)
  legend('topright',legend=c(expression(italic('p(B<B'['MSY']*')')),
                                expression(italic('p(B<B'['MSY']*' historical)')),
                                #expression(italic('p(SSB<SSB'['MSY']*')')),
                                #expression(italic('p(SSB<SSB'['MSY']*' historical)')),
                                expression(italic('p(U<U'['MSY']*')'))),bty='n',
         lty=c(1,2,1,2,1),col=c('black','black','red'))#blue','blue','red'))
  mtext(side=2,'Proportion of Stocks',line=2.5)

