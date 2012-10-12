# Function to calculate average of columns given a column index (by)
# Returns a matrix with column names as the by's and rows as averaged values for each wl
# Input column 1 should be wavelengths
agg <- function(x, by = stop("by is needed"), FUN = stop("FUN is needed")) {
wl <- x[,1]
x <- x[,-1]
by0 <- by  # retain original values
by <- as.numeric(factor(by))  # allow for numeric, character data
dat <- matrix(data = NA, nrow = nrow(x), ncol = length(unique(by)))
for (i in seq(along = unique(by))) {
	if (is.null(dim(x[,by==i]))) {
	dat[,i] <- apply(cbind(x[,by==i], x[,by==i]), 1, FUN)  # apply can't avg single columns
	}
	else dat[,i] <- apply(x[,by==i], 1, FUN)
}
colnames(dat) <- unique(by0)
as.data.frame(cbind(wl=wl, dat))
}
