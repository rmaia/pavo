# input spectra with rows as specs and columns as wavelengths
# fun options = min, max, stretch, sum, [bin?], [diffs?]
procspec <- function(Y, f=NULL, sm=FALSE, spar=NULL, span=NULL, method="spline") {
wl <- Y[, 1]
Y <- Y[, -1]

# smoothing
if (sm==F) Y2 <- Y
else {
	if (method=="spline")
		Y2 <- sapply(row.names(Y), function(z){smooth.spline(x=wl, y=Y[z,], spar=spar)$y})
	if (method=="loess")
		Y2 <- sapply(row.names(Y), function(z){loess.smooth(x=wl, y=Y[z,], span=span, 
								 degree=2, family="gaussian", evaluation = length(wl))$y})
}

# normalization
if (is.null(f)) Y2 <- Y
if (f=="min") Y2 <- sapply(1:ncol(Y2), function(z)Y2[,z] - min(Y2[,z]))
if (f=="max") Y2 <- sapply(1:ncol(Y2), function(z)Y2[,z] - max(Y2[,z]))
if (f=="sum") Y2 <- sapply(1:ncol(Y2), function(z)Y2[,z] / sum(Y2[,z]))
if (f=="stretch") {
	Y2 <- sapply(1:ncol(Y2), function(z)Y2[,z] - min(Y2[,z]))
	Y2 <- sapply(1:ncol(Y2), function(z)Y2[,z] / max(Y2[,z]))
}

#if (f=="diff") Y2 <- (Y2 - Y2[, 1]) / Y2[, 1]

Y2 <- as.data.frame(cbind(wl, Y2))

class(Y2) <- c('rspec', 'data.frame')

Y2

}


# testing zone
#tmp <- procspec(rspecs, f="stretch")
#plot(tmp, p=8:15, type='h')
