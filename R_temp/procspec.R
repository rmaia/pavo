# input spectra with rows as specs and columns as wavelengths
# fun options = min, max, stretch, sum, [bin?], [diffs?]
procspec <- function(Y, f=NULL, sm=FALSE, spar=NULL, span=NULL, method="spline") {
wl <- Y[1, ]
Y <- Y[-1, ]

# smoothing
if (sm==F) Y2 <- Y
else {
	if (method=="spline")
		Y2 <- t(sapply(row.names(Y), function(z){smooth.spline(x=wl, y=Y[z,], spar=spar)$y}))
	else if (method=="loess")
		Y2 <- t(sapply(row.names(Y), function(z){loess.smooth(x=wl, y=Y[z,], span=span, 
						degree=2, family="gaussian", evaluation=length(wl))$y}))
	}

# normalization
if (is.null(f)) Y2 <- Y
else if (f=="min") {Y2 <- Y2 - apply(Y2, 1, min)}
else if (f=="max") {Y2 <- Y2 / apply(Y2, 1, max)}
else if (f=="sum") {Y2 <- Y2 / apply(Y2, 1, sum)}
else if (f=="stretch") {
		Y2 <- Y2 - apply(Y2, 1, min)
		Y2 <- Y2 / apply(Y2, 1, max)
	}
#if (f=="diff") Y2 <- (Y2 - Y2[, 1]) / Y2[, 1]

rbind(wl, Y2)

}
