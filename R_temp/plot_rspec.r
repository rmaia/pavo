# Pavo function to plot groups of spectra
# p is 'picked' spectra; can be a numeric vector or factor (e.g., sex=="male")
# for heatplots, by is a vector describing numeric values across which spectra are varying

# TODO: 
# 1. add way to plot mean+/-sd plots
# 2. add interpolating argument for heatmap
# 3. add additional title, axes, color arguments

plot.rspec <- function(specs, p = NULL, type = "overlay", by = NULL, col=heat.colors(25)) {
	type <- match.arg(type, c("heatmap", "overlay", "stack"))
	wl <- specs[, 1]
	if (is.logical(p)) p <- which(p=="TRUE")
	if (is.null(p)) p <- 2:ncol(specs)
	specs <- specs[, p]

	if (type=="heatmap") {
		if (is.null(by))
			by <- 1:ncol(specs)
		dat <- sapply(1:nrow(specs), function(z){approx(x = by, y = specs[z, ], n = 100)$y})
		image(x = wl, y = approx(by, n = 100)$y, z = t(dat), col = col)
	}
	
	if (type=="overlay") {
		plot(specs[, 1]~wl, type = 'l', ylim = c(min(specs), max(specs)))
		for (i in 2:ncol(specs)) lines(specs[, i]~wl)
	}
	
	if (type=="stack") {
		specs2 <- sapply(1:ncol(specs), function(z){specs[, z] - min(specs[, z])})
		ym <- apply(specs2, 2, max)  
		plot(specs2[, 1], ylim=c(0, sum(ym)), type='l')
		for (i in 2:ncol(specs)) {
			lines(specs2[, i] + cumsum(ym)[i - 1])
		}
	}
}
