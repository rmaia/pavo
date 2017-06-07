#' Retrieve or plot in-built data
#' 
#' Retrieve (as an rspec object) or plot pavo's in-built spectral data.
#'
#' @param visual visual systems. Options are:
#' \itemize{
#' \item \code{'none'}: no visual sensitivity data.
#' \item \code{'all'}: no visual sensitivity data.
#' \item \code{'apis'}: Honeybee \emph{Apis mellifera} visual system.
#' \item \code{'avg.uv'}: average avian UV system.
#' \item \code{'avg.v'}: average avian V system.
#' \item \code{'bluetit'}: Blue tit \emph{Cyanistes caeruleus} visual system.
#' \item \code{'canis'}: Canid \emph{Canis familiaris} visual system.
#' \item \code{'cie2'}: 2-degree colour matching functions for CIE models of human 
#'  colour vision. Functions are linear transformations of the 2-degree cone fundamentals 
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' \item \code{'cie10'}: 10-degree colour matching functions for CIE models of human 
#'  colour vision. Functions are linear transformations of the 10-degree cone fundamentals 
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' \item \code{'musca'}: Housefly \emph{Musca domestica} visual system.
#' \item \code{'pfowl'}: Peafowl \emph{Pavo cristatus} visual system.
#' \item \code{'star'}: Starling \emph{Sturnus vulgaris} visual system.
#' }
#' @param achromatic the sensitivity data used to calculate luminance (achromatic)
#'  receptor stimulation. Options are: 
#' \itemize{
#'  \item \code{'none'}: no achromatic sensitivity data.
#'  \item \code{'all'}: all achromatic sensitivity data.
#'	\item \code{'bt.dc'}: Blue tit \emph{Cyanistes caeruleus} double cone.
#'  \item \code{'ch.dc'}: Chicken \emph{Gallus gallus} double cone.
#'  \item \code{'st.dc'}: Starling \emph{Sturnus vulgaris} double cone.
#'  \item \code{'md.r1'}: Housefly \emph{Musca domestica} R1-6 photoreceptor.
#' }
#' @param illum illuminants. Options are:
#' \itemize{ 
#' \item \code{'none'}: no illuminant data.
#' \item \code{'all'}: no background spectral data.
#' \item \code{'bluesky'} open blue sky.
#' \item \code{'D65'}: standard daylight.
#' \item \code{'forestshade'} forest shade.
#' }
#' @param bkg background spectra. Options are:
#' \itemize{ 
#' \item \code{'none'}: no background spectral data.
#' \item \code{'all'}: no background spectral data.
#' \item \code{'green'}: green foliage.
#' }
#' @param trans Ocular transmission data. Options are:
#' \itemize{ 
#' \item \code{'none'}: no transmission data.
#' \item \code{'all'}: all transmission data.
#' \item \code{'bluetit'}: blue tit \emph{Cyanistes caeruleus} 
#' ocular transmission (from Hart et al. 2000).
#' \item \code{'blackbird'}: blackbird \emph{Turdus merula} 
#' ocular transmission (from Hart et al. 2000).
#' }
#' @param plot should the spectral data be plotted, or returned instead (defaults to \code{FALSE})? 
#' @param ... additional graphical options passed to \code{\link{plot.rspec}} when \code{plot = TRUE}.
#'
#' @return An object of class \code{rspec} (when \code{plot = FALSE}), containing 
#' a wavelength column \code{'wl'} and spectral data binned at 1 nm intervals from 300-700 nm. 
#'
#' @examples \dontrun{
#' # Plot the honeybee's receptors
#' sensdata(visual = 'apis', ylab = 'Absorbance', plot = TRUE)
#' 
#' # Plot the vverage UV vs V avian receptors
#' sensdata(visual = c('avg.v', 'avg.uv'), ylab = 'Absorbance', plot = TRUE)
#' 
#' # Retrieve the CIE colour matching functions as an rspec object
#' ciedat <- sensdata(visual = c('cie2', 'cie10'))
#' }
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' 
#' @export
#' 

sensdata <- function(
    visual = c('none', 'all', 'avg.uv', 'avg.v', 'bluetit', 'star', 'pfowl', 'apis', 'canis', 'cie2', 'cie10', 'musca'),
    achromatic = c('none', 'all', 'bt.dc','ch.dc', 'st.dc', 'md.r1'),
    illum = c('none', 'all', 'bluesky', 'D65', 'forestshade'), 
    trans = c('none', 'all', 'bluetit', 'blackbird'),
    bkg = c('none', 'all', 'green'),
    plot = FALSE,
    ...){
  
  visual2 <- match.arg(visual, several.ok = TRUE)
  achro2 <- match.arg(achromatic, several.ok = TRUE)
  illum2 <- match.arg(illum, several.ok = TRUE)
  bkg2 <- match.arg(bkg, several.ok = TRUE)
  trans2 <- match.arg(trans, several.ok = TRUE)
  
  dat <- data.frame(wl = 300:700)
  
  # Visual system
  if(!isTRUE('none' %in% visual2)){
    if(isTRUE('all' %in% visual2))
      visual2 <- c('avg.uv', 'avg.v', 'bluetit', 'star', 'pfowl', 'apis', 'canis', 'cie2', 'cie10', 'musca')
    sens <- as.data.frame(vissyst)
    S <- as.data.frame(subset(sens, select = grepl(paste(visual2, collapse = "|"), names(sens))))
    dat <- cbind(dat, S)
  }
  
  # Achromatic receptor
  if(!isTRUE('none' %in% achro2)){
    if(isTRUE('all' %in% achro2))
      achro2 <- c('bt.dc','ch.dc', 'st.dc', 'md.r1')
   sens <- as.data.frame(vissyst)
   achro <- as.data.frame(subset(sens, select = grepl(paste(achro2, collapse = "|"), names(sens))))
   dat <- cbind(dat, achro)
  }
  
  # Illuminant
  if(!isTRUE('none' %in% illum2)){
    if(isTRUE('all' %in% illum2))
      illum2 <- c('bluesky', 'D65', 'forestshade')
   bgil <- as.data.frame(bgandilum)
   illum <- as.data.frame(subset(bgil, select = grepl(paste(illum2, collapse = "|"), names(bgil))))
   dat <- cbind(dat, illum)
  }

  # Background
  if(!isTRUE('none' %in% bkg2)){
    if(isTRUE('all' %in% bkg2))
      bkg2 <- 'green'
   bgil <- as.data.frame(bgandilum)
   bkg <- as.data.frame(subset(bgil, select = grepl(paste(bkg2, collapse = "|"), names(bgil))))
   dat <- cbind(dat, bkg)
  }

  # Transmission
  if(!isTRUE('none' %in% trans2)){
    if(isTRUE('all' %in% trans2))
      trans2 <- c('bluetit', 'blackbird')
   trdat <- as.data.frame(transmissiondata)
   trans <- as.data.frame(subset(trdat, select = grepl(paste(trans2, collapse = "|"), names(trdat))))
   dat <- cbind(dat, trans)
  }
  
  dat <- suppressMessages(as.rspec(dat))
  
  if(plot == TRUE)
    plot(dat, ...)
  else
    dat
  
}
