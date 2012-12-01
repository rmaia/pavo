data(sicalis)
bysic <- gsub("^ind[0-9].",'',names(sicalis)[-1])

aggplot(sicalis,bysic)

plot(meanplotspecs[,2]~meanplotspecs[,1], ylim=c(0,50), type='l')
polygon( c(meanplotspecs[,1],rev(meanplotspecs[,1])),
		c(meanplotspecs[,2]+sdplotspecs[,2],rev(meanplotspecs[,2]-sdplotspecs[,2]) ),
		border=NA, col='darkgrey', alpha=0.1
)

aggplot <- function(rspecdata, by, FUN.center=mean, FUN.error=sd, xlim=NULL, ylim=NULL, shadecol=NULL, lcol=NULL, lty=NULL, alpha=0.2,  ...){

#take aggregated data
cntplotspecs <- aggspec(rspecdata,by=by, FXN=FUN.center)
errplotspecs <- aggspec(rspecdata, by=by, FXN=FUN.error)

# make wavelength vector
wl_index <- which(names(rspecdata)=='wl')
wl_index_cnt <- which(names(cntplotspecs)=='wl')
wl_index_err <- which(names(errplotspecs)=='wl')

if (length(wl_index) > 0) {
  haswl <- TRUE
  wl <- rspecdata[, wl_index]
  cntplotspecs <- as.data.frame(cntplotspecs[,-wl_index_cnt])
  errplotspecs <- as.data.frame(errplotspecs[,-wl_index_err])
} else {
  haswl <- FALSE
  wl <- 1:nrow(rspecdata)
  warning('No wavelengths provided; using arbitrary index values')
}

indexsub <- 1:dim(cntplotspecs)[2]

polygon_data <- sapply(indexsub, function(x) 
			c(cntplotspecs[,x]+errplotspecs[,x], rev(cntplotspecs[,x]-errplotspecs[,x]) )
			)

polygon_wl <- c(wl,rev(wl))

# set limits
if (is.null(xlim))
  xlim <- range(wl)

if (is.null(ylim))
  ylim <- range(polygon_data)


# coloring for overlay plot & others

if (length(lty) < ncol(cntplotspecs))
  lty <- rep(lty, ncol(cntplotspecs))

if (length(shadecol) < ncol(cntplotspecs))
  shadecol <- rep(shadecol, ncol(cntplotspecs))
if (any(class(shadecol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
  shadecol <- spec2rgb(cbind(wl,cntplotspecs))

if (length(lcol) < ncol(cntplotspecs))
  lcol <- rep(lcol, ncol(cntplotspecs))
if (any(class(lcol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
  lcol <- spec2rgb(cbind(wl,cntplotspecs))
   
col_list <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

col_list <- rep(col_list, length=dim(cntplotspecs)[2])

if(is.null(shadecol))
  shadecol <- col_list

if(is.null(lcol))
  lcol <- col_list
 
 shadecol = rgb(t(col2rgb(shadecol))/255, alpha=alpha)
 lcol = rgb(t(col2rgb(lcol))/255)

  plot(cntplotspecs[, 1]~wl, type = 'n', # c(min(rspecdata), max(rspecdata)), 
       xlab = 'Wavelength (nm)', ylab = 'Reflectance (%)', xlim = xlim, ylim = ylim,
       ...)
  polygon(polygon_data[,1]~polygon_wl, col=shadecol[1], border=NA)
  lines(cntplotspecs[,1]~wl, col=lcol[1], lty=lty[1], ...)
  
  if (ncol(cntplotspecs)>1) {
    for (i in 2:ncol(cntplotspecs)){
  polygon(polygon_data[,i]~polygon_wl, col=shadecol[i], border=NA)
  lines(cntplotspecs[,i]~wl, col=lcol[i], lty=lty[i], ...)
  }
  }


}

aggplot(sicalis,bysic)
aggplot(sicalis,bysic, shade=spec2rgb(sicalis),lcol=1)
aggplot(sicalis,bysic,lcol=1, FUN.error=function(x)sd(x)/sqrt(length(x)))
