fao <- read.csv('data/fao_data.csv')

year      <- fao$sustainable_year
sustain   <- approx(x=fao$sustainable_year,y=fao$sustainable,xout=1980:2017)$y/100
overfish  <- 1 - sustain
underfish <- approx(x=fao$underfishing_years,y=fao$underfished,xout=1980:2017)$y/100
goodfish  <- sustain - underfish

par(mfrow=c(1,1))
plot(fao$sustainable_year,fao$sustainable,type='l',ylim=c(0,100),bty='n',xaxt='n',xlab='',ylab='')
lines(fao$sustainable_year[1:23],fao$underfished[1:23],type='l',ylim=c(0,100))
	polygon(x=c(fao$sustainable_year[1:23],rev(fao$sustainable_year[1:23])),y=c(rep(100,23),rev(fao$sustainable[1:23])),col=adjustcolor('red',alpha.f=0.5))
	polygon(x=c(fao$sustainable_year[1:23],rev(fao$sustainable_year[1:23])),y=c(fao$underfished[1:23],rev(fao$sustainable[1:23])),col=adjustcolor('yellow',alpha.f=0.5))
	polygon(x=c(fao$sustainable_year[1:23],rev(fao$sustainable_year[1:23])),y=c(fao$underfished[1:23],rep(0,23)),col=adjustcolor('green',alpha.f=0.5))
	axis(side=1,at=seq(1975,2017,2))
	mtext(adj=0.1,'FAO Stock Status')
	mtext(side=2,line=2.5,'% of Global Stocks Assessed')
	text(x=2005,y=90,'Overfished')
	text(x=2005,y=60,'Maximally Fished')
	text(x=2005,y=8,'Under Fished')

