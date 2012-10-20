plot.spec.curves <- function (filename, specreps) {

x<- read.table (filename, sep=",", header=T)

plotnumber <- 1
nplots <- ((ncol(x)-1)/specreps)
par(mfrow=c(3,4),ask=TRUE)

for (i in 1:nplots){

	bloc <-x[,(((plotnumber-1)*specreps)+2):((plotnumber*specreps)+1)]
	specs <- c(1:ncol(bloc))
	yaxismin <-min(bloc)
	yaxismax <-max(bloc)
	plot(x[,1],bloc[,1],cex=0.1,ylim=c(yaxismin,yaxismax+5),col=1,xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (280,yaxismax+6,legend=names(bloc),cex=0.7,text.col = specs,bty="n", xjust=0)

nextplot = 2
		while (nextplot < ncol(bloc)+1) { 						
			lines (x[,1],bloc[,nextplot],cex=0.1, col = nextplot)
			nextplot<- nextplot+1}

plotnumber=plotnumber+1}
}

