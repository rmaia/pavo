FWHM <- function(y, bounds=c(300,700), plot=T) {
  wl <- bounds[1]:bounds[2]
	y2 <- y[(bounds[1]-299) : (bounds[2]-299)]
	Yi <- max(y2)
	Yj <- min(y2)
  Xi <- which(y2==Yi)  # lambda_max
	fsthalf <- y2[1:Xi]
	sndhalf <- y2[Xi+1:length(y2)]
	halfmax <- (Yi+Yj)/2  # reflectance midpoint
	fstHM <- which.min(abs(fsthalf-halfmax))
	sndHM <- which.min(abs(sndhalf-halfmax))
	Xa <- wl[fstHM]
	Xb <- wl[Xi+sndHM]
  if(plot==T) {
    plot(y~c(300:700),type='l')
	  abline(v=wl[y2==Yi], col="red")
	  abline(h=halfmax, col="red")
	  abline(v=Xa, col="red", lty=2)
	  abline(v=Xb, col="red", lty=2)
	  hue=wl[Xi]
	  Xc <- min(hue-Xa, Xb-hue, na.rm=T)
	  return(list(l=Xa, r=Xb, fwhm=Xb-Xa, hue=hue, b3=Yi))
  }
  else {
	  hue <- wl[Xi]
	  Xc <- min(hue-Xa, Xb-hue, na.rm=T)
	  return(c(l=Xa, r=Xb, fwhm=Xb-Xa, hue=hue, b3=Yi))
  }
}
