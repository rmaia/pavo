  
nms <- names(rspecdata)

wl_index <- which(names(rspecdata)=='wl')
if (length(wl_index) > 0) {
  haswl <- TRUE
  wl <- rspecdata[, wl_index]
} else {
  haswl <- FALSE
  wl <- 1:nrow(rspecdata)
  warning('No wavelengths provided; using arbitrary index values', call.=FALSE)
}

# set default wavelength range if not provided
if (is.null(lim)) {
  lim <- c(head(wl,1), tail(wl,1))
}

# subset based on indexing vector
if (is.logical(select))
  select <- which(select=='TRUE')
if (is.null(select)&haswl==TRUE)
  select <- (1:ncol(rspecdata))[-wl_index]
if (is.null(select)&haswl==FALSE)
  select <- 1:ncol(rspecdata)

rspecdata <- as.data.frame(rspecdata[, select, drop=FALSE])


wlrange <- lim[1]:lim[2]


  
  
  rspecdata2 <- rspecdata[(which(wl==lim[1])):(which(wl==lim[2])), ]  # working wl range
  Yi <- apply(rspecdata2, 2, max)  # max refls
  Yj <- apply(rspecdata2, 2, min)  # min refls
  Yk <- apply(rspecdata, 2, min)  # min refls, whole spectrum
  Xi <- sapply(rspecdata2, which.max)  # lambda_max index
  # CE edit: test if any wls have equal reflectance values
  dblpeaks <- sapply(Xi, length)
  dblpeak.nms <- nms[select][dblpeaks>1]
  if (any(dblpeaks>1)) {
    # Keep only first peak of each spectrum
    Xi <- sapply(Xi, "[[", 1)
    warning(paste("Multiple wavelengths have the same reflectance value (", paste(dblpeak.nms, collapse=", "), "). Using first peak found. Please check the data or try smoothing.", sep=""), call.=FALSE)
  }
  # end CE edit
  
  # minimum to the right and left of the peak
  dimrspd <- dim(rspecdata2)
  minleft <- sapply(seq(dimrspd[2]), function(x) 
      which.min(rspecdata2[1:Xi[x],x]))
  minright <- sapply(seq(dimrspd[2]), function(x) 
      which.min(rspecdata2[Xi[x]:dimrspd[1],x]) +Xi[x] - 1)
      
  # make a symmetric curve based on side of the curve that has minimum
  
  leftbrighter <- sapply(seq(dimrspd[2]), function(x)
      rspecdata2[minleft[x],x] > rspecdata[minright[x], x] )
  
  rspsym <- vector('list', length=dimrspd[2])
  
  for(i in seq(dimrspd[2])){
  	if(leftbrighter[i]){
  		rspsym[[i]] <- rspecdata2[c(minright[i]:Xi[i],Xi[i]:minright[i]), i]
  	}
  	
  	if(!leftbrighter[i]){
  		rspsym[[i]] <- rspecdata2[c(minleft[i]:Xi[i],Xi[i]:minleft[i]), i]
  	}
  } 
    
  # remove minimum from curves
  nrspsym <- lapply(seq_along(rspsym), function(x) rspsym[[x]] - Yj[x])
  
  # fit normal distribution
  fitnorm <- lapply(nrspsym, function(x) 
  nls(x ~ k*exp(-1/2*(seq_along(x)-mu)^2/ssq), 
    start=c(mu=mean(seq_along(x)), ssq=length(x), k=3), control=list(warnOnly=TRUE))
  )
  
  # calculate FWHM
  FWHM <- unlist(lapply(fitnorm, function(x)
  2.35482*sqrt(summary(x)$coefficients['ssq',1])
  ))

if(absolute.min)
    Yj <- Yk


### PLOTTING
plotindex <- vector('list', length=dimrspd[2])

for(i in seq(dimrspd[2])){
  	if(leftbrighter[i]){
  		plotindex[[i]] <- seq_along(nrspsym[[i]]) + minright[i] - length(nrspsym[[i]])
  	}
  	
  	if(!leftbrighter[i]){
  		plotindex[[i]] <- seq_along(nrspsym[[i]]) + minleft[i]
  	}
  }
  
plotline <- lapply(seq(dimrspd[2]), function(x) predict(fitnorm[[x]]) + Yj[x])

for(i in 1:12){
	plot(rspecdata2[,i], type='l')
	lines(I(nrspsym[[i]]+ Yj[i])~plotindex[[i]], col='red', lwd=0.7)
	lines(plotline[[i]]~plotindex[[i]], lty=2)
}  

