# input an rspec object or data frame w/spectra in columns and wavelengths in rows
# options = min, max, stretch, sum, bin, center (subtract avg. from all specs)

# TODO
# 1. add way to handle negatives

procspec <- function(specs, opt = c('none', 'smooth', 'maximum', 'minimum', 'stretch', 
										 'bin', 'fix', 'sum', 'center'), method = c('loess', 'spline'), 
										 spar = .65, span = .15, bins = 20) {

	wl_index <- which(names(specs)=='wl')
	if (length(wl_index > 0)){
		wl <- specs[, wl_index]
		specs <- specs[, -wl_index]
		} else {
			specs <- specs
			}

	opt <- match.arg(opt, several.ok = TRUE)  	# 'fix' = how to handle negative values
	method <- match.arg(method)

	if (any(opt=='none')) {
		print('No relevant processing option entered. Returning raw values') 
		specs <- as.data.frame(cbind(wl, specs))
		class(specs) <- c('rspec', 'data.frame')
		return(specs)
	}
	
	if (any(opt=='smooth')&method=='spline')
		specs <- sapply(names(specs), function(z){smooth.spline(x = wl, y = specs[, z], 
										spar = spar)$y})

	if (any(opt=='smooth')&method=='loess')
		specs <- sapply(names(specs), function(z){loess.smooth(x = wl, y = specs[, z], 
										span = span, degree = 2, family = "gaussian", 
										evaluation = length(wl))$y})

	if (any(opt=='minimum'))
		specs <- sapply(1:ncol(specs), function(z)specs[, z] - min(specs[, z]))

	if (any(opt=='maximum'))
		specs <- sapply(1:ncol(specs), function(z)specs[, z] / max(specs[, z]))

	if (any(opt=='sum'))
		specs <- sapply(1:ncol(specs), function(z)specs[, z] / sum(specs[, z]))

	if (any(opt=='stretch')) {
		specs <- sapply(1:ncol(specs), function(z)specs[, z] - min(specs[, z]))
		specs <- sapply(1:ncol(specs), function(z)specs[, z] / max(specs[, z]))
	}

	if (any(opt=='center'))
		specs <- sapply(1:ncol(specs), function(z)specs[, z] - mean(specs[, z]))

#	if (any(opt=='fix'))
#		specs <- ...
		# option 1 = set all negatives to zero
		# option 2 = add absolute val of most neg value to all spectra
		# option 3 = set negatives to NAs, then smooth?

	# Calculate medians according to # of bins specified for use in PCA
	# Method follows Cuthill et al. (1999)
	if (any(opt=='bin')) {
		bw <- floor(length(wl)/bins)
		wl_bin <- seq(head(wl,1), tail(wl,1), by=bw)
		wl_ind <- match(wl_bin, wl)
		specs <- sapply(1:(length(wl_ind)-1), function(z) 
									  apply(specs[wl_ind[z]:(wl_ind[z]+bw), ], 2, median))
	}

	if (any(opt=='bin'))
		{specs <- as.data.frame(cbind(wl_bin[-length(wl_bin)], t(specs)))}
		else {specs <- as.data.frame(cbind(wl, specs))}
	
	class(specs) <- c('rspec', 'data.frame')
	
	specs

}

# testing zone
#tmp <- procspec(rspecs)
#tmp <- procspec(rspecs, f="stretch")
#plot(tmp, p=8:15, type='h')
