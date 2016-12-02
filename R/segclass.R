#' Segment classification
#'
#' Calculates segment classification measures as defined in Endler (1990)
#' @param rspecdata (required) a data frame, such as objects of class 
#' \code{rspec}, with a column containing wavelength range, named 'wl'
#' and spectra data in remaining columns. 
#' @param range vector of length 2 indicating the lower and upper wavelength bounds used
#' to calculate segments (defaults to 300nm to 700nm).
#' @return A data frame with LM and MS segment classification scores.
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' segclass(sicalis) }
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}
#' @references Endler, J. A. (1990) On the measurement and classification of 
#' color in studies of animal color patterns. Biological Journal of the Linnean 
#' Society, 41, 315-352.

segclass <- function (rspecdata, range=c(300,700)) {

wl_index <- which(names(rspecdata)=='wl')
wl <- rspecdata[,wl_index]
lambdamin <- min(wl)
rspecdata <- rspecdata[,-wl_index]

segmts <- trunc(as.numeric(quantile(range[1]:range[2])))

Q1 <- which(wl==segmts[1]):which(wl==segmts[2])
Q2 <- which(wl==segmts[2]):which(wl==segmts[3])
Q3 <- which(wl==segmts[3]):which(wl==segmts[4])
Q4 <- which(wl==segmts[4]):which(wl==segmts[5])

Q1mat <- rspecdata[Q1, ]
Q2mat <- rspecdata[Q2,]
Q3mat <- rspecdata[Q3, ]
Q4mat <- rspecdata[Q4, ]

B1 <- apply(rspecdata,2,sum)
Q1score <- apply(Q1mat,2,sum)/B1
Q2score <- apply(Q2mat,2,sum)/B1
Q3score <- apply(Q3mat,2,sum)/B1
Q4score <- apply(Q4mat,2,sum)/B1

LM <- Q4score-Q2score
MS <- Q3score-Q1score

segclassdata <- data.frame(LM,MS, row.names = names(rspecdata))

segclassdata

}
