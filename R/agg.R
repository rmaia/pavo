# Function to calculate average of columns given a column index (by)
# Returns a matrix with column names as the by's and rows as averaged values for each wl
# Input column 1 should be wavelengths

# RM tip: Always provide useful defaults. If you don't want to provide one 
#     (i.e. there is no meaningful default), leave arg empty. Default is to return 
#     error. But if there's an implemented default (i.e. for FUN), use it.

agg <- function(x, by, FXN = mean) {

#BEGIN RM EDIT
# check: user may have removed 'wl' function already.
# (data.frame doesn't allow duplicate names anymore, so this should work)

wl_index <- which(names(x)=='wl')

if(length(wl_index>0)){
	wl <- x[,wl_index]
	y <- x[,-wl_index]
	}else{
		y <- x
		}

# retain original values
by0 <- by

#BEGIN RM EDIT
# Allow for means of every "by" specs, if "by" is a single number
# i.e. if by=3, average every 3 consecutive specs of "x"

if(length(by)==1){
	by0 <- names(y)[seq(1,length(names(y)),by=by)]
	by <- rep(1:(length(y)/by),each=by)
}
#END RM EDIT

#BEGIN RM EDIT
# check: does x have the same number of columns as the by vector?

if(dim(y)[2]!=length(by)) 
stop(paste('\n',dQuote(deparse(substitute(by))),'is not of same length as columns in',dQuote(deparse(substitute(x)))))

#END RM EDIT

by <- factor(by)

dat <- sapply(levels(by),function(z)apply(y[which(by==z)],1,FXN))

colnames(dat) <- unique(by0)
data.frame(cbind(wl=wl, dat))
}
