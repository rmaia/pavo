#CREATE COOL ANIMATED PLOTS

#THIS CREATES A NEW FILE FOR EACH 'i' IN THE LOOP
for (i in 40:55){
jpeg(paste("fig",i,".jpg", sep=""))
plot(vbst[1:401,i]~vbst[1:401,1], type='l', ylim=c(0,70), xlim=c(300,700), xlab="Wavelength (nm)", ylab="Reflectance (%)", col=rgb(spec2rgb(vbst[1:401,i])), lwd=3, xaxs='i', yaxs='i')
polygon(x=c(300:700, 700:300), y=c(vbst[1:401,i], rep(0,401)), border=NA, col=rgb(spec2rgb(vbst[1:401,i]), alpha=.3))
dev.off()
}

#next, in terminal use convert -delay 50 -loop 50 fig*.jpg animated.gif