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



plot.spec.curves <- function (specdata, specreps=1, lwd=2) {


if (specreps <= 0) stop ("Invalid specreps value")

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
specdata <- specdata[,-wl_index]


nplots <- dim(specdata)[1]/specreps

if(specreps <=4){
  par(mfrow=c(3,4),ask=TRUE)
  yaxismult <- c(0,1.4)
  }

if (specreps > 4) {
  par(mfrow=c(2,3),ask=TRUE)
  yaxismult <- c(0.9,1.4)
  }
  	
if (specreps > 7) {
  par(mfrow=c(2,2),ask=TRUE)
  yaxismult <- c(0.9,1.8)
  }

if (specreps > 9) {
  par(mfrow=c(1,2),ask=TRUE)
  yaxismult <- c(0.9,1.4)
  }

if (specreps > 12){
  par(mfrow=c(1,1),ask=TRUE)
  yaxismult <- c(0.9,1.8)
  }

col_list=brewer.pal(8,'Set1')
#col_list = c("#666666", "#1B9E77", "#D95F02", "#7570B3", 
#             "#E7298A", "#66A61E", "#E6AB02", "#A6761D")

for (i in 1:nplots){
  if (specreps == 1) {
  	bloc <- data.frame(specdata[i])
  	col_list <- 'black' }else{
      bloc <- specdata[,(((i-1)*specreps)+1):(i*specreps)]
  	  }

  yaxislims = c(min(bloc),max(bloc))*yaxismult
  
  leg <- names(bloc)
  legcolor <- rep(col_list, length=dim(bloc)[2])

	
	plot(wl, bloc[,1], col=legcolor[1] , ylim=yaxislims, type='l', lwd=lwd,
	     xlab="Wavelength (nm)",ylab="% Reflectance")

  if(dim(bloc)[2] > 1)
    for(j in 2:dim(bloc)[2])
      lines(wl, bloc[,j], col=legcolor[j], type='l', lwd=lwd)
	
  legend('topright', legend=names(bloc), cex=0.7, bty="n", 
         text.col=legcolor)	
	}
}



