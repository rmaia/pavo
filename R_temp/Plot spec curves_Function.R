## TODO (Pierre): Put nicer colours!
## TODO (Pierre): Arg to change number of figures per plot
## TODO (Pierre): Test
## TODO (Pierre): Error handling
## TODO (Pierre): Documentation

plot.spec.curves <- function (specdata, specreps=0) {

if (specreps == 0) stop ("Need to include number of spectral curves per figure")



nplots <- ((ncol(specdata)-1)/specreps)
par(mfrow=c(3,4),ask=TRUE)

for (i in 1:nplots){
	
	if (specreps == 1) bloc <- specdata[, i+1]
	if (specreps > 1) bloc <-specdata[,(((i-1)*specreps)+2):((i*specreps)+1)]
	leg <- names(specdata[,2:dim(specdata)[2]])
	yaxismin <-min(bloc)
	yaxismax <-max(bloc)
	
	if (specreps == 1){
	plot(specdata[,1],bloc,cex=0.1,ylim=c(yaxismin,yaxismax+5),col=1,xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (280,yaxismax+6,legend=leg[i],cex=0.7,bty="n", xjust=0)	
	}
	
	if (specreps > 1){
	specs <- c(1:ncol(bloc))
	plot(specdata[,1],bloc[,1],cex=0.1,ylim=c(yaxismin,yaxismax+5),col=1,xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (280,yaxismax+6,legend=names(bloc),cex=0.7,bty="n", xjust=0,text.col = specs)
	

	nextplot = 2
		while (nextplot < ncol(bloc)+1) { 						
			lines (specdata[,1],bloc[,nextplot],cex=0.1, col = nextplot)
			nextplot<- nextplot+1}
}
}
}

plot.spec.curves(specdata)

