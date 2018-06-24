#' Plot spectral curves
#'
#' Plots one or multiple spectral curves in the same graph to rapidly 
#' compare groups of spectra.
#'
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec}
#' that has wavelength range in the first column, named 'wl', and spectral measurements in the 
#' remaining columns. 
#' @param by number of spectra to include in each graph (defaults to 1)
#' @param scale defines how the y-axis should be scaled. \code{'free'}: panels can vary in
#' the range of the y-axis; \code{'equal'}: all panels have the y-axis with the same range.
#' @param legpos legend position control. Either a vector containing \code{x} and \code{y} coordinates
#' or a single keyword from the list: \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, 
#' \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"}.
#' @param ... additional parameters to be passed to plot
#' 
#' @return Spectral curve plots
#' 
#' @note Number of plots presented per page depends on the number of graphs produced.
#' 
#' @export
#' 
#' @examples \dontrun{
#' data(sicalis)
#' explorespec(sicalis, 3)
#' explorespec(sicalis, 3, ylim = c(0, 100), legpos = c(500, 80))}
#' 
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}

explorespec <- function (rspecdata, by = NULL, scale = c('equal', 'free'), legpos = 'topright', ...){

#oPar <- par(no.readonly=TRUE)
oPar <- par('mar','oma', 'ask','mfrow')
on.exit(par(oPar))

wl_index <- which(names(rspecdata)=='wl')
wl <- rspecdata[, wl_index]
rspecdata <- rspecdata[, -wl_index, drop=FALSE]
leg2 <- names(rspecdata)

scale <- match.arg(scale)

## begin CE edit

by0 <- by

# default by value
if (is.null(by))
  by <- 1

# check if the by argument has a 'wl' entry (e.g. if names were obtained through
# regex conditions on the original spec names) and remove it
if (length(which(by=='wl'))!=0)
  by <- by[-which(by=='wl')]
# Handle when 'by' is a list of factors
# if (is.list(by)) {
#   wl_id <- sapply(1:length(by), function(x) which(by[[x]]=='wl'))  # extract wl columns
#   # remove 'wl' column from each vector in list
#   if (any(sapply(wl_id, length)!=0)) {
#     id <- which(sapply(wl_id, length)!=0)
#     by[id] <- lapply(by[id], "[", -unlist(wl_id)[id])
#   }
#   # check that wl column is the same for all vectors
#   if (length(unique(wl_id))==1) {
#     by <- do.call('paste', c(by, sep='.'))
#   } else {
#     stop("mismatch in column names of input vectors")
#   }
# }
# Allow for means of every "by" data, if "by" is a single number
# i.e. if by=3, average every 3 consecutive data of "data"
if (length(by) == 1){
  nms <- names(rspecdata)[seq(1, length(names(rspecdata)), by = by)]
  by <- rep(1:(length(rspecdata) / by), each = by)
}
# check: does data have the same number of columns as the by vector?
# TODO: this is actually alright if you are not plotting by a character vector
# describing the species, type of patch, etc. (by=3 should work fine here, last
# plot will only have fewer lines)
if (dim(rspecdata)[2]!=length(by) & !is.integer(by)) 
  stop(paste('\n',dQuote(deparse(substitute(by))), 'is not of same length as columns in',
             dQuote(deparse(substitute(data)))))

by <- factor(by)

# return(by)

# number of 'by' groups
numby <- length(levels(by))

# by <- as.numeric(by)

if (numby <= 0) stop("Invalid by value")  # is this needed anymore?

# nplots <- ceiling(dim(rspecdata)[2] / numby)

nplots <- numby

##### end CE edit

# setup multi-panel plot parameters
if (numby <=4)
  yaxismult <- c(0,1.4)
if (numby > 4)
  yaxismult <- c(0.9,1.4)
if (numby > 7)
  yaxismult <- c(0.9,1.8)
if (numby > 9)
  yaxismult <- c(0.9,1.4)
if (numby > 12)
  yaxismult <- c(0.9,1.8)

if (nplots == 1)
  par(mfrow=c(1,1))
if (nplots == 2)
  par(mfrow=c(1,2), mar = c(5, 4, 4, 0.5) + 0.1)
if (nplots > 2 & nplots < 5)
  par(mfrow=c(2,2), mar = c(5, 4, 0.5, 0.5) + 0.1)
if (nplots >= 5 & nplots < 7)
  par(mfrow=c(2,3), mar = c(5, 4, 0.5, 0.5) + 0.1)
if (nplots >= 7 & nplots < 9)
  par(mfrow=c(2,4), mar = c(5, 4, 0.5, 0.5) + 0.1)
if (nplots >= 9)
  par(mfrow=c(3,4), mar = c(5, 4, 0.5, 0.5) + 0.1)
if (nplots > 12)
  par(ask=TRUE)

arg <- list(...)

arg$x <- wl

if (is.null(arg$col)) {
  col_list <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
  } else {
  col_list <- arg$col
  }

par(mar=c(2, 2, 1, 1), oma = c(3, 3, 0, 0))

if (all(is.null(arg$ylim), scale=='equal')) {
  arg$ylim <- c(min(rspecdata), max(rspecdata))*yaxismult
}




# TODO: should be able to deal with this if there are NAs in the by vector
if (anyNA(by)) {
  warning("NA values in by vector. please check.")
}



# Do the plotting
for (i in 1:nplots) {
  if (numby == 1) {
  	bloc <- data.frame(rspecdata[i])
    } else {
      # THIS IS WHAT I NEED TO FIX
      bloc <- rspecdata[, which(by==levels(by)[i])]
#       bloc <- rspecdata[, by==unique(by)[i]]
#       bloc <- rspecdata[,(((i-1)*numby)+1):min(i*numby,dim(rspecdata)[2])]
    }

  # SET OPTIONAL ARGUMENTS    
  if (all(is.null(arg$ylim), scale=='free'))
    arg$ylim <- c(min(bloc), max(bloc))*yaxismult

  if (is.null(arg$type))
    arg$type <- 'l'
  
  if (is.null(arg$ylab))
    arg$ylab <- '% Reflectance'
  
  if (is.null(arg$xlab))
    arg$xlab <- 'Wavelength (nm)'
  
  if (length(legpos)>1) {
    legx <- legpos[1]
    legy <- legpos[2]
    } else {
      legx <- legpos
      legy <- NULL
    }
  
  leg <- names(bloc)
  
  if (!is.null(dim(bloc))) {
    legcolor <- rep(col_list, length=dim(bloc)[2])
    } else {
    legcolor <- col_list
    }

  if (!is.null(dim(bloc))) {
  	arg$y <- bloc[,1]
    if(is.null(arg$col)) {
      arg$col <- legcolor
    }
	  do.call(plot,arg)
    } else {
      arg$col <- legcolor[1]
      arg$y <- bloc
      do.call(plot, arg)
      legend(x=legx, y=legy, legend=leg2[i], cex=0.9, bty="n", 
         text.col=legcolor)
      }
  if (numby == 1) {
    legend(x=legx, y=legy, legend=leg2[i], cex=0.9, bty="n", 
           text.col=legcolor)
  }
  if (!is.null(dim(bloc))) {
    if (dim(bloc)[2] > 1) {
      for(j in 2:dim(bloc)[2]) {
        arg$y <- bloc[ ,j]
        arg$col <- legcolor[j]
        do.call(lines, arg)
      }
      legend(x=legx, y=legy, legend=names(bloc), cex=0.9, bty="n", text.col=legcolor)
    }
  }
  arg$col <- legcolor[1]
  if (scale=='free')
    arg$ylim <- NULL
  if (i %% 12 == 0) {
    mtext(arg$xlab, side=1, outer=TRUE, line=1)
    mtext(arg$ylab, side=2, outer=TRUE, line=1)	
  }
}

if (i %% 12 != 0){
  mtext(arg$xlab, side=1, outer=TRUE, line=1)
  mtext(arg$ylab, side=2, outer=TRUE, line=1)
}

# do we need this still?
if ((dim(rspecdata)[2]/numby) != round((dim(rspecdata)[2]/numby))){
  warning("by is not a factor of the number of column in rspecdata")
}

}