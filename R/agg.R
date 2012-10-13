# Function to calculate average of columns given a column index (by)
# Returns a matrix with column names as the by's and rows as averaged values for each wl
# Input column 1 should be wavelengths

# RM tip: Always provide useful defaults. If you don't want to provide one 
#     (i.e. there is no meaningful default), leave arg empty. Default is to return 
#     error. But if there's an implemented default (i.e. for FUN), use it.

agg <- function(x, by, FUN = mean) {

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

# check: does x have the same number of columns as the by vector?

if(dim(y)[2]!=length(by)) 
stop(paste('\n',dQuote(deparse(substitute(by))),'is not of same length as columns in',dQuote(deparse(substitute(x)))))

#END RM EDIT

# retain original values
by0 <- by
# allow for numeric, character data
by <- as.numeric(factor(by))  

dat <- matrix(data = NA, nrow = nrow(y), ncol = length(unique(by)))

for (i in seq(along = unique(by))) {
# apply can't avg single columns
	if (is.null(dim(y[,by==i]))) {
	   dat[,i] <- apply(cbind(y[,by==i], y[,by==i]), 1, FUN)
	}else{
		dat[,i] <- apply(y[,by==i], 1, FUN)
		}
}

colnames(dat) <- unique(by0)
data.frame(cbind(wl=wl, dat))
}

