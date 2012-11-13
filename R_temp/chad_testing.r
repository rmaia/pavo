#### TESTING

install.packages("/Users/chad/Documents/pavo", type='source', repos=NULL)
require(pavo)
specs <- getspec("~/Documents/School/PhD/Projects/Polyplectron/data/2012-05-10/pheasants_200-1000nm", ext="ttt")

plot(specs[,2]~specs[,1], type='l')
#spec1 <- specs  		# rows as wavelengths
spec2 <- t(specs)  	# columns as wavelengths

tmp <- procspec(spec2, f="stretch", sm=T, method="loess", span=.25, spar=.25)
plot(tmp[6,]~tmp[1,], type='l')
lines(tmp[6,])
for (i in 3:10) lines(tmp[i,]~tmp[1,])
abline(h=c(0,1), lty=2)


# TEST RGB FUNCTION

plot(spec2[2,]~c(300:700), col=rgb(spec2rgb(spec2[2,])), type='l', lwd=3, ylim=c(0,100))
lines(spec2[2,]+6~c(300:700), col=rgb(spec2rgb(spec2[2,]+6)), type='l', lwd=3)

spec2rgb(spec2[2,]+7)


# testing zone for new plotting function
plot(rspecs, p=c(2,14,9), type="h", by=c(10,20,30))
plot(witu, p=2:7, type="h", by=seq(10,60,by=10))
plot(rspecs, p=c(2,14,9,12,13,19), type="o", by=c(10,20,30))
plot(rspecs, p=2:4, type='s')
plot(rspecs, p=dat$spp=="gallus", type='o')
