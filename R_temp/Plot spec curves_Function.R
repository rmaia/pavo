## TODO (Pierre): Allow function to plot 1 curve at a time
## TODO (Pierre): Put nicer colours!
## TODO (Pierre): Arg to change number of figures per plot
## TODO (Pierre): Test
## TODO (Pierre): Error handling
## TODO (Pierre): Documentation

plot.spec.curves <- function (specdata, specreps=0) {

if (specreps == 0) stop ("Need to include number of spectral curves per figure")


plotnumber <- 1
nplots <- ((ncol(specdata)-1)/specreps)
par(mfrow=c(3,4),ask=TRUE)

for (i in 1:nplots){

	bloc <-specdata[,(((plotnumber-1)*specreps)+2):((plotnumber*specreps)+1)]
	specs <- c(1:ncol(bloc))
	yaxismin <-min(bloc)
	yaxismax <-max(bloc)
	plot(specdata[,1],bloc[,1],cex=0.1,ylim=c(yaxismin,yaxismax+5),col=1,xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (280,yaxismax+6,legend=names(bloc),cex=0.7,text.col = specs,bty="n", xjust=0)

nextplot = 2
		while (nextplot < ncol(bloc)+1) { 						
			lines (specdata[,1],bloc[,nextplot],cex=0.1, col = nextplot)
			nextplot<- nextplot+1}

plotnumber=plotnumber+1}
}

plot.spec.curves(specdata)

