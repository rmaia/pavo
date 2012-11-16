#' Plot spectral curves
#'
#' Plots one or multiple spectral curves in the same graph to rapidly 
#' compare groups of spectra.
#'
#' @param specdata(required) a data frame, possibly an object of class \code{rspec}
#' that has wavelength range in the first column, named 'wl', and spectral measurements in the 
#' remaining columns. 
#' @param specreps number of spectra to include in each graph (defaults to 1)
#' @param lwd Width of the lines displayed on the plots (defaults to 2)
#' @param scale defines how the y-axis should be scaled. \code{'free'}: panels can vary in
#' the range of the y-axis; \code{'equal'}: all panels have the y-axis with the same range.
#' @return Spectral curve plots
#' @note Number of plots presented per page depends on the number of curves per graph.
#' @export
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}



explorespec <- function (specdata, specreps=1, lwd=2, scale=c('free','equal')) {


if (specreps <= 0) stop ("Invalid specreps value")

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
specdata <- specdata[,-wl_index]
leg2 <- names(specdata)
if ((dim(specdata)[2]/specreps) != round((dim(specdata)[2]/specreps))){
  warning("specreps is not a factor of the number of column in specdata")
}

scale <- match.arg(scale)

nplots <- ceiling(dim(specdata)[2]/specreps)

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


col_list <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

for (i in 1:nplots){
  if (specreps == 1) {
  	bloc <- data.frame(specdata[i])
  	col_list <- 'black' }else{
      bloc <- specdata[,(((i-1)*specreps)+1):min(i*specreps,dim(specdata)[2])]
  	  }

  if(scale=='free')
    yaxislims <- c(min(bloc), max(bloc))*yaxismult

  if(scale=='equal')
    yaxislims <- c(min(specdata), max(specdata))*yaxismult

  leg <- names(bloc)
  
  if (!is.null(dim(bloc))){
  legcolor <- rep(col_list, length=dim(bloc)[2])} else {
  legcolor <- col_list
  }

  if (!is.null(dim(bloc))){
	plot(wl, bloc[,1], col=legcolor[1] , ylim=yaxislims, type='l', lwd=lwd,
	     xlab="Wavelength (nm)",ylab="% Reflectance")}else{
      plot(wl, bloc, col=legcolor[1] , ylim=yaxislims, type='l', lwd=lwd,
	     xlab="Wavelength (nm)",ylab="% Reflectance")
      legend('topright', legend=leg2[length(leg2)], cex=0.9, bty="n", 
         text.col=legcolor)
      }
 if (specreps == 1){
     legend('topright', legend=leg2[i], cex=0.9, bty="n", 
         text.col=legcolor)}
  if(!is.null(dim(bloc))){
    if (dim(bloc)[2] > 1){
    for(j in 2:dim(bloc)[2]){
      lines(wl, bloc[,j], col=legcolor[j], type='l', lwd=lwd)
  legend('topright', legend=names(bloc), cex=0.9, bty="n", 
         text.col=legcolor)	}}}
   
	}
}



