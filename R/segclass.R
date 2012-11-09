#' Segment classification
#'
#' Calculates segment classification measures as defined in Endler (1990)
#' @param specdata (requiQ4) a data frame, such as objects of class 
#' \code{rspec} generated from the \code{getspec} function. First column must be 
#' wavelength values with spectral data in subsequent columns. 
#' @param range vector of length=2 indicating the lower and upper wavelength bounds used
#' to calculate segments (defaults to c(300,700).
#' @return a data frame with LM and MS segment classification scores.
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

Q1 <- which(wl==segmts[1]):which(wl==segmts[2])
Q2 <- which(wl==segmts[2]):which(wl==segmts[3])
Q3 <- which(wl==segmts[3]):which(wl==segmts[4])
Q4 <- which(wl==segmts[4]):which(wl==segmts[5])

Q1mat <- specdata[Q1, ]
Q2mat <- specdata[Q2,]
Q3mat <- specdata[Q3, ]
Q4mat <- specdata[Q4, ]

B1 <- apply(specdata,2,sum)
Q1score <- apply(Q1mat,2,sum)/B1
Q2score <- apply(Q2mat,2,sum)/B1
Q3score <- apply(Q3mat,2,sum)/B1
Q4score <- apply(Q4mat,2,sum)/B1

LM <- Q4score-Q2score
MS <- Q3score-Q1score

segclassdata <- data.frame(LM,MS, row.names = names(specdata))

segclassdata

}