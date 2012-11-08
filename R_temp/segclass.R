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

segclass <- function (specdata, range=c(300,700)) {

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
lambdamin <- min(wl)
specdata <- specdata[,-wl_index]

segmts <- trunc(as.numeric(quantile(range[1]:range[2])))

    UV <- which(wl==segmts[1]):which(wl==segmts[2])
  Blue <- which(wl==segmts[2]):which(wl==segmts[3])
 Green <- which(wl==segmts[3]):which(wl==segmts[4])
   Red <- which(wl==segmts[4]):which(wl==segmts[5])

   UVmat <- specdata[UV, ]
 Bluemat <- specdata[Blue,]
Greenmat <- specdata[Green, ]
  Redmat <- specdata[Red, ]

B1 <- apply(specdata,2,sum)
UVscore <- apply(UVmat,2,sum)/B1
Bluescore <- apply(Bluemat,2,sum)/B1
Greenscore <- apply(Greenmat,2,sum)/B1
Redscore <- apply(Redmat,2,sum)/B1

LM <- Redscore-Bluescore
MS <- Greenscore-UVscore

segclassdata <- data.frame(LM,MS, row.names = names(specdata))

segclassdata

}