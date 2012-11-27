#' Aggregate reflectance spectra
#' 
#' Combines spectra (by taking the average, for example) according to 
#' an index or a vector of identities.
#' 
#' @param data (required) data frame containing the spectra to be manipulated. If
#' it contains a wavelength column containing "wl", that column will be ignored.
#' @param by (required) either a single value specifying the range of spectra within
#' the data frame to be combined (for example, \code{by} = 3 indicates the function
#' will be applied to groups of 3 consecutive columns in the spectra data frame) or
#' a vector containing identifications for the columns in the spectra data frame
#' (in which case the function will be applied to each group of spectra sharing the
#' same identification).
#' @param FXN the function to be applied to the groups of spectra. (defaults to \code{mean})
#' @return A data frame of class \code{rspec} containing the spectra after applying the aggregating function.
#' @export
#' @examples \dontrun{
#' specs <- getspec('/examplespec')
#' specs <- data.frame(cbind(specs, specs[,-1],specs[,-1],specs[,-1]))
#' aggspec(specs,by=2)
#' aggspec(specs,by=c('a','b','a','b','a','b','a','b')) }
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


# Function to calculate average of columns given a column index (by)
# Returns a matrix with column names as the by's and rows as averaged values for each wl
# Input column 1 should be wavelengths

# RM tip: Always provide useful defaults. If you don't want to provide one 
#     (i.e. there is no meaningful default), leave arg empty. Default is to return 
#     error. But if there's an implemented default (i.e. for FUN), use it.

aggspec <- function(data, by, FXN = mean) {

#BEGIN RM EDIT
# check: user may have removed 'wl' function already.
# (data.frame doesn't allow duplicate names anymore, so this should work)

wl_index <- which(names(data)=='wl')

if(length(wl_index>0)){
	wl <- data[,wl_index]
	y <- data[,-wl_index]
	}else{
		y <- data
		}

#BEGIN RM EDIT 2
# check if the by argument has a 'wl' entry (e.g. if names were obtained through
# regex conditions on the original spec names) and remove it

if(length(which(by=='wl'))!=0)
  by<- by[-which(by=='wl')]

#END RM EDIT 2


# retain original values
by0 <- by

#BEGIN RM EDIT 1
# Allow for means of every "by" data, if "by" is a single number
# i.e. if by=3, average every 3 consecutive data of "data"

if(length(by)==1){
	by0 <- names(y)[seq(1,length(names(y)),by=by)]
	by <- rep(1:(length(y)/by),each=by)
}
#END RM EDIT 1

#BEGIN RM EDIT 3
# check: does data have the same number of columns as the by vector?

if(dim(y)[2]!=length(by)) 
stop(paste('\n',dQuote(deparse(substitute(by))),'is not of same length as columns in',dQuote(deparse(substitute(data)))))

#END RM EDIT 3

by <- factor(by)

dat <- sapply(levels(by),function(z)apply(y[which(by==z)],1,FXN))

colnames(dat) <- unique(by0)
res<- data.frame(cbind(wl=wl, dat))
class(res) <- c('rspec','data.frame')
res
}
