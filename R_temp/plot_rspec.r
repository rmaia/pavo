# Pavo function to plot groups of spectra
# p is 'picked' spectra; can be a numeric vector or factor (e.g., sex=="male")
# for heatmaps, 'by' is a vector describing numeric values across which spectra are varying
# 'n' is number of bins to interpolate 'varying' over

# TODO: 
# 1. add additional title, axes, color arguments

plot.rspec <- function(specs, p = NULL, type = c("overlay", "stack", "heatmap"), 
											 varying = NULL, by = NULL, col = heat.colors(25), rows = 2, 
											 n = 100) {

	old.par <- par(no.readonly = TRUE)  # all par settings that could be set

	type <- match.arg(type)

	wl_index <- which(names(data)=='wl')
	if (length(wl_index > 0)) {
		wl <- specs[, wl_index]
		specs <- as.data.frame(specs[, -wl_index])
	} else {
		specs <- as.data.frame(specs)
		}

	# subset based on indexing vector
	if (is.logical(p)) p <- which(p=="TRUE")
	if (is.null(p)) p <- 1:ncol(specs)
	specs <- as.data.frame(specs[, p])

	if (type=="heatmap") {
		if (is.null(varying))
			varying <- 1:ncol(specs)
		dat <- sapply(1:nrow(specs), function(z){approx(x = varying, y = specs[z, ], 
								  n = n)$y})
		image(x = wl, y = approx(varying, n = n)$y, z = t(dat), col = col,
					xlab = "Wavelength (nm)")
	}

	if (type=="overlay") {
		plot(specs[, 1]~wl, type = 'l', ylim = c(min(specs), max(specs)), 
				 xlab = "Wavelength (nm)", ylab = "Reflectance (%)")
		if (ncol(specs)>1) for (i in 2:ncol(specs)) lines(specs[, i]~wl)
	}

	if (type=="stack") {
		specs2 <- sapply(1:ncol(specs), function(z){specs[, z] - min(specs[, z])})
		ym <- apply(specs2, 2, max)  
		plot(specs2[, 1], ylim=c(0, sum(ym)), type='l', xlab = "Wavelength (nm)")
		for (i in 2:ncol(specs)) {
			lines(specs2[, i] + cumsum(ym)[i - 1])
		}
	}

	par(old.par)

}

# Testing zone for new plotting function

#plot(rspecs, p=c(2,14,9), type="h", by=c(10,20,30))
#plot(witu, p=2:7, type="h", varying=seq(10,60,by=10))
#plot(rspecs, p=c(2,14,9,12,13,19), type="o", by=c(10,20,30))
#plot(rspecs, p=2:4, type='s')
#rspecs2 <- procspec(rspecs, f="stretch")
#plot(rspecs2, p=2:8, type='o')
#plot(rspecs, by=spp, type='g', rows=3)
