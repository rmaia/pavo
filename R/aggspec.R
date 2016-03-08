#' Aggregate reflectance spectra
#' 
#' Combines spectra (by taking the average, for example) according to 
#' an index or a vector of identities.
#' 
#' @param rspecdata (required) data frame, possibly of class \code{rspec}
#' containing the spectra to be manipulated. If
#' it contains a wavelength column containing "wl", that column will be ignored.
#' @param by (required) either a single value specifying the range of spectra within
#' the data frame to be combined (for example, \code{by} = 3 indicates the function
#' will be applied to groups of 3 consecutive columns in the spectra data frame); 
#' a vector containing identifications for the columns in the spectra data frame
#' (in which case the function will be applied to each group of spectra sharing the
#' same identification); or a list of vectors, e.g., \code{by = list(sex, species)}.
#' @param FUN the function to be applied to the groups of spectra. (defaults to \code{\link{mean}})
#' @param trim logical. if \code{TRUE} (default), the function will try to identify and 
#' remove numbers at the end of the names of the columns in the new rspec object.
#' @return A data frame of class \code{rspec} containing the spectra after applying the aggregating function.
#' @export
#' @examples \dontrun{
#' data(teal)
#' # Average every two spectra
#' teal.sset1 <- aggspec(teal, by = 2)
#' plot(teal.sset1)
#' # Create factor and average spectra by levels 'a' and 'b'
#' ind <- rep(c('a','b'), times=6)
#' teal.sset2 <- aggspec(teal, by=ind)
#' plot(teal.sset2) }
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


# Function to calculate average of columns given a column index (by)
# Returns a matrix with column names as the by's and rows as averaged values for each wl
# Input column 1 should be wavelengths

# RM tip: Always provide useful defaults. If you don't want to provide one 
#     (i.e. there is no meaningful default), leave arg empty. Default is to return 
#     error. But if there's an implemented default (i.e. for FUN), use it.

aggspec <- function(rspecdata, by = NULL, FUN = mean, trim = TRUE) {

#BEGIN RM EDIT
# check: user may have removed 'wl' function already.
# (data.frame doesn't allow duplicate names anymore, so this should work)

wl_index <- which(names(rspecdata)=='wl')

if (length(wl_index>0)){
	wl <- rspecdata[, wl_index]
	y <- rspecdata[, -wl_index]
	} else {
		  y <- rspecdata
		}

if (is.null(by)) {
  dat <- apply(y, 1, FUN)
  res <- data.frame(cbind(wl=wl, dat))
  class(res) <- c('rspec','data.frame')
  return(res)
}

if (is.numeric(by))
    if (ncol(y) %% by != 0)
        stop('by not a multiple of number of spectra')

#BEGIN RM EDIT 2
# check if the by argument has a 'wl' entry (e.g. if names were obtained through
# regex conditions on the original spec names) and remove it

if (length(which(by=='wl'))!=0)
  by <- by[-which(by=='wl')]

#END RM EDIT 2

# Handle when 'by' is a list of factors
if (is.list(by)) {
  wl_id <- sapply(1:length(by), function(x) which(by[[x]]=='wl'))  # extract wl columns
  # remove 'wl' column from each vector in list
    if (any(sapply(wl_id, length)!=0)) {
      id <- which(sapply(wl_id, length)!=0)
      by[id] <- lapply(by[id], "[", -unlist(wl_id)[id])
    }
  # check that wl column is the same for all vectors
    if (length(unique(wl_id))==1) {
      by <- do.call('paste', c(by, sep='.'))
    } else {
          stop("mismatch in column names of input vectors")
      }
}

# retain original 'by' values
by0 <- by

#BEGIN RM EDIT 1
# Allow for means of every "by" data, if "by" is a single number
# i.e. if by=3, average every 3 consecutive data of "data"
if (length(by)==1){
	by0 <- names(y)[seq(1, length(names(y)), by = by)]
	by <- rep(1:(length(y) / by), each = by)
}
#END RM EDIT 1

#BEGIN RM EDIT 3
# check: does data have the same number of columns as the by vector?

if (dim(y)[2]!=length(by)) 
stop(paste('\n',dQuote(deparse(substitute(by))),'is not of same length as columns in',dQuote(deparse(substitute(data)))))

#END RM EDIT 3

# Add ability to aggregate based on multiple vectors (given a list as input)
# TODO: add that list can be an input in roxygen doc 

by <- factor(by)  # is this necessary?

dat <- sapply(unique(by), function(z){apply(y[which(by==z)], 1, FUN)})

colnames(dat) <- unique(by0)

if(trim)
  colnames(dat) <- gsub('[\\. | \\_ | \\-][0-9]*$', '', colnames(dat))

res <- data.frame(cbind(wl=wl, dat))

class(res) <- c('rspec','data.frame')

res

# This would return list of spectral data and data.frame with metadata:
  # set <- strsplit(names(res), split='\\.')
  # out <- as.data.frame(do.call("rbind", set))
  # list(res, out)

}
