#' Segment classification
#'
#' Calculates segment classification measures as defined in Endler (1990)
#' @param specdata (required) a data frame, such as objects of class 
#' \code{rspec} generated from the \code{getspec} function. First column must be 
#' wavelength values with spectral data in subsequent columns. 
#' @return a data frame with LM and MS segment classification scores
#' @export
#' @author Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
#' @references Endler, J. A. (1990) On the measurement and classification of 
#' color in studies of animal color patterns. Biological Journal of the Linnean 
#' Society, 41, 315-352.

segclass <- function (specdata) {

lambdamin <- all.specs[1, 1]

if (lambdamin != 300 & lambdamin != 320) {
  stop ("Minimum wavelength must be 300nm or 320nm")
  }

if (dim(all.specs)[1] != 401 & dim(all.specs)[1] != 381) {
  stop ("Wavelength length must be 401 or 381")
  }

if (lambdamin == 300){
UV <- c(1:101)
Blue <- c(101:201)
Green <- c(201:301)
Red <- c(301:401)
}

if (lambdamin == 320) {
UV <- c(1:96)
Blue <- c(96:191)
Green <- c(191:286)
Red <- c(286:381)
}

alldat <- specdata[, 2:dim(specdata)[2]]
UVmat <- specdata[UV, 2:dim(specdata)[2]]
Bluemat <- specdata[Blue, 2:dim(specdata)[2]]
Greenmat <- specdata[Green, 2:dim(specdata)[2]]
Redmat <- specdata[Red, 2:dim(specdata)[2]]

B1 <- apply(alldat,2,sum)
UVscore <- apply(UVmat,2,sum)/B1
Bluescore <- apply(Bluemat,2,sum)/B1
Greenscore <- apply(Greenmat,2,sum)/B1
Redscore <- apply(Redmat,2,sum)/B1

LM <- Redscore-Bluescore
MS <- Greenscore-UVscore

segclassdata <- as.data.frame(matrix(c(LM,MS),nrow=dim(specdata)[2]-1,ncol=2),
  row.names = names(specdata)[2:dim(specdata)[2]])
names(segclassdata) <- c("LM","MS")

return(segclassdata)
}