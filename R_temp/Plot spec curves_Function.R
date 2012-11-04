## TODO (Pierre): Fix the colors
## 2 Problems: 1) Colors in the first graph is different than the other graphs
## This problem occurs in the first run only, or when changing the number of curves per plots
## 2) Colours are terrible. Tried using RColorBrewer but keep getting error message even using example

#' Plot spectral curves
#'
#' Plots multiple spectral curves in the same graph to rapidly compare groups of spectra.
#'
#' @param specdata(required) a data frame, possibly an object of class \code{rspec}
#' that has wavelength range in the first column and spectral measurements in the 
#' remaining columns. 
#' @param specreps number of spectra to include in each graph (defaults to 1)
#' @return Spectral curve plots
#' @note Number of plots presented per page depends on the number of curves per graph.
#' @export
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}



plot.spec.curves <- function (specdata, specreps=1) {


if (specreps == 0) stop ("Number of spectral curves cannot be 0")
if (specreps < 0) stop ("Number of spectral curves cannot be negative")

nplots <- ((ncol(specdata)-1)/specreps)
par(mfrow=c(3,4),ask=TRUE)
if (specreps > 4) par(mfrow=c(2,3))
if (specreps > 7) par(mfrow=c(2,2))
if (specreps > 9) par(mfrow=c(1,2))
if (specreps > 12) par(mfrow=c(1,1))

for (i in 1:nplots){
	leg <- names(specdata[,2:dim(specdata)[2]])
	if (specreps == 1) bloc <- specdata[, i+1]
	
	if (specreps > 1) {
	bloc <-specdata[,(((i-1)*specreps)+2):((i*specreps)+1)]
	legcolor <- palette(rainbow(n=ncol(bloc), start = 0.7, end = 0.1))
	}
	yaxismin <-min(bloc)
	yaxismax <-max(bloc)
	
	if (specreps == 1){
	plot(specdata[,1],bloc,cex=0.1,ylim=c(yaxismin,yaxismax+5),col=1,xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (specdata[1, 1]-20,yaxismax+6,legend=leg[i],cex=0.7,bty="n", xjust=0, text.col=1)	
	}
	
	if (specreps > 1){
	plot(specdata[,1],bloc[,1],cex=0.1,ylim=c(yaxismin,yaxismax+5),col=legcolor[1],xlab="Wavelength (nm)",ylab="% Reflectance")
	legend (specdata[1, 1]-20,yaxismax+6,legend=names(bloc),cex=0.7,bty="n", xjust=0, text.col = legcolor)
	
	nextplot = 2
		while (nextplot < ncol(bloc)+1) { 						
			lines (specdata[,1],bloc[,nextplot],cex=0.1, col = legcolor[nextplot])
			nextplot<- nextplot+1}
			
		}
	}
}



