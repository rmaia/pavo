#' Visual models
#' 
#' Calculates quantum catches at each photoreceptor. Both raw and relative values 
#' can be returned, for use in a suite of colorspace and non-colorspace models. 
#' 
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec}
#'  that has wavelength range in the first column, named 'wl', and spectral measurements in the 
#'  remaining columns. 
#' @param qcatch Which quantal catch metric to return. Options are:
#' \itemize{
#' \item \code{'Qi'}: Quantum catch for each photoreceptor 
#' \item \code{'fi'}: Quantum catch according to Fechner law (the signal of the receptor
#'  channel is proportional to the logarithm of the quantum catch)
#' \item \code{'Ei'}: Hyperbolic-transformed quantum catch, where Ei = Qi / (Qi + 1).
#' }
#' @param visual the visual system to be used. Options are:
#' \itemize{
#'	\item a data frame such as one produced containing by \code{sensmodel}, containing 
#'    user-defined sensitivity data for the receptors involved in colour vision. 
#'    The data frame must contain a \code{'wl'} column with the range of wavelengths included, 
#'    and the sensitivity for each other cone as a column.
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
#' \item \code{'segment'}: Generic tetrachromat 'viewer' for use in the segment analysis of Endler (1990).
#' \item \code{'star'}: Starling \emph{Sturnus vulgaris} visual system.
#' }
#' @param achromatic the sensitivity data to be used to calculate luminance (achromatic)
#'  receptor stimulation. Either a vector containing the sensitivity for a single receptor, 
#'  or one of the options: 
#' \itemize{
#'  \item \code{'none'}: no achromatic stimulation calculated
#'	\item \code{'bt.dc'}: Blue tit \emph{Cyanistes caeruleus} double cone
#'  \item \code{'ch.dc'}: Chicken \emph{Gallus gallus} double cone
#'  \item \code{'st.dc'}: Starling \emph{Sturnus vulgaris} double cone
#'  \item \code{'md.r1'}: Housefly \emph{Musca domestica} R1-6 photoreceptor
#'  \item \code{'ml'}: the summed response of the two longest-wavelength photoreceptors
#'  \item \code{'l'}: the longest-wavelength photoreceptor
#'  \item \code{'all'}: the summed response of all photoreceptors
#' }
#' @param illum either a vector containing the illuminant, or one of the options:
#' \itemize{ 
#' \item \code{'ideal'}: homogeneous illuminance of 1 accross wavelengths (default)
#' \item \code{'bluesky'} open blue sky.
#' \item \code{'D65'}: standard daylight.
#' \item \code{'forestshade'} forest shade.
#' }
#' @param bkg background spectrum. Note that this will have no effect when \code{vonkries = FALSE}. 
#' Either a vector containing the spectral data, or one of the options:
#' \itemize{ 
#' \item \code{'ideal'}: homogeneous illuminance of 1 accross all wavelengths (default).
#' \item \code{'green'}: green foliage.
#' }
#' @param trans either a vector containing the ocular or environmental transmission
#' spectra, or one of the options:
#' \itemize{ 
#' \item \code{'ideal'}: homogeneous transmission of 1 accross all wavelengths (default)
#' \item \code{'bluetit'}: blue tit \emph{Cyanistes caeruleus} 
#' ocular transmission (from Hart et al. 2000).
#' \item \code{'blackbird'}: blackbird \emph{Turdus merula} 
#' ocular transmission (from Hart et al. 2000).
#' }
#' @param relative should relative quantum catches be returned (i.e. is it a color
#'  space model? Defaults to \code{TRUE}).
#' @param vonkries logical. Should the von Kries color correction transformation be applied?
#'  (defaults to \code{FALSE}).
#' @param scale a value by which the illuminant will be multiplied. Useful for when the 
#'  illuminant is a relative value (i.e. transformed to a maximum of 1 or to a percentage),
#'  and does not correspond to quantum flux units ($umol*s^-1*m^-2$). Useful values
#'  are, for example, 500 (for dim light) and 10000 (for bright illumination). Note that if
#' \code{vonkries = TRUE} this transformation has no effect.
#'
#' @return An object of class \code{vismodel} containing the photon catches for each of the 
#'  photoreceptors considered. Information on the parameters used in the calculation are also
#'  stored and can be called using the \code{summary.vismodel} function.
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Dichromat (dingo)
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colspace(vis.flowers, space = 'di')
#' 
#' # Trichromat (honeybee)
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' tri.flowers <- colspace(vis.flowers, space = 'tri')
#' 
#' # Tetrachromat (blue tit)
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'bluetit')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' }
#' 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. 
#'  (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative 
#'  Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S., Partridge, J. C., Cuthill, I. C., Bennett, A. T. D. (2000). 
#' Visual pigments, oil droplets, ocular media and cone photoreceptor distribution in two
#' species of passerine bird: the blue tit (Parus caeruleus L.) and the blackbird 
#' (Turdus merula L.).
#'  Journal of Comparative Physiology A, 186, 375-387.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress 
#'  In Retinal And Eye Research, 20(5), 675-703.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage 
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings. 
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns 
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of 
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of 
#'  the middle- and long-wavelength sensitive cones derived from measurements in 
#'  observers of known genotype. Vision Research, 40, 1711-1737.
#' @references CIE (2006). Fundamental chromaticity diagram with physiological axes. 
#'  Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the Commission 
#'  Internationale de l' Eclairage.

vismodel <- function(rspecdata, 
  visual = c('avg.uv', 'avg.v', 'bluetit', 'star', 'pfowl', 'apis', 'canis', 'cie2', 'cie10', 'musca', 'segment'), 
  achromatic = c('none', 'bt.dc','ch.dc', 'st.dc','ml', 'l', 'md.r1', 'all'),
  illum = c('ideal', 'bluesky', 'D65', 'forestshade'), 
  trans = c('ideal', 'bluetit', 'blackbird'),
  qcatch = c('Qi', 'fi', 'Ei'),
  bkg = c('ideal', 'green'), 
  vonkries = FALSE, scale = 1, relative = TRUE)
{
  
# remove & save colum with wavelengths

wl_index <- which(names(rspecdata)=='wl')
wl <- rspecdata[,wl_index]
y <- rspecdata[, -wl_index, drop=FALSE]

# Negative value check
if(length(y[y < 0]) > 0){
  warning(paste("The spectral data contain ", length(y[y < 0]), " negative value(s), which may produce unexpected results. Consider using procspec() to correct them."))
}

# in case rspecdata only has one spectrum
# 01/10/2017: drop=TRUE above should fix it
#if(is.null(dim(y))){
#  y <- data.frame(rspecdata[,-wl_index])
#  names(y) <- names(rspecdata)[-wl_index]
#  }

visual2 <- try(match.arg(visual), silent = T)
sens <- vissyst
achromatic2 <- try(match.arg(achromatic), silent=T)
illum2 <- try(match.arg(illum), silent=T)
bg2 <- try(match.arg(bkg), silent=T)
tr2 <- try(match.arg(trans), silent=T)

if(class(achromatic2) == 'try-error')
  if(is.logical(achromatic))
    if(FALSE %in% achromatic){
      achromatic <- 'none'
      achromatic2 <- 'none'
    }

qcatch <- match.arg(qcatch)

# Defaults for CIE-specific stuff. Bit ugly - better way to do this? 
if(substr(visual2, 1, 3) == 'cie'){ 
  if(!vonkries) {
    vonkries <- TRUE
    warning('cie system chosen, overriding vonkries to TRUE', call.=FALSE)
  }
  
  if(relative){
    relative <- FALSE
    warning('cie system chosen, overriding relative to FALSE', call.=FALSE)
  }
  
  if(achromatic2 != 'none'){
  	achromatic2 <- 'none'
  	warning('cie system chosen, overriding achromatic to none', call.=FALSE)
  }
  
  if(qcatch != 'Qi'){
  	qcatch <- 'Qi'
  	warning('cie system chosen, overriding qcatch to Qi', call.=FALSE)
  }  
}

# Defaults for segment analysis stuff. 
if(visual2 == 'segment'){ 
  if(vonkries){
    vonkries <- FALSE
    warning('segment analysis chosen, overriding vonkries to FALSE', call.=FALSE)
  }
  
  if(!relative){
    relative <- TRUE
    warning('segment analysis chosen, overriding relative to TRUE', call.=FALSE)
  }
  
  if(achromatic2 != 'all'){
    achromatic2 <- 'all'
    warning('segment analysis chosen, overriding achromatic to all', call.=FALSE)
  }
  
  if(qcatch != 'Qi'){
    qcatch <- 'Qi'
    warning('segment analysis chosen, overriding qcatch to Qi', call.=FALSE)
  }
  
  if(bg2 != 'ideal'){
    bg2 <- 'ideal'
    warning('segment analysis chosen, overriding bkg to ideal', call.=FALSE)
  }
  
  if(tr2 != 'ideal'){
    tr2 <- 'ideal'
    warning('segment analysis chosen, overriding trans to ideal', call.=FALSE)
  }
  
  if(illum2 != 'ideal'){
    illum2 <- 'ideal'
    warning('segment analysis chosen, overriding illum to ideal', call.=FALSE)
  }
}

# Grab the visual system
if(visual2 == 'segment'){  # make a weird custom 'visual system' for segment analysis
  S <- data.frame(matrix(0, nrow = length(wl), ncol = 4))
  names(S) <- c('S1', 'S2', 'S3', 'S4')
  segmts <- trunc(as.numeric(quantile(min(wl):max(wl))))
  
  S[wl %in% segmts[1]:segmts[2] ,1] <- 1
  S[wl %in% segmts[2]:segmts[3] ,2] <- 1
  S[wl %in% segmts[3]:segmts[4] ,3] <- 1
  S[wl %in% segmts[4]:segmts[5] ,4] <- 1

  sens_wl <- wl
  
}else if(!inherits(visual2,'try-error')){
    visual <- match.arg(visual)
    S <- sens[,grep(visual,names(sens))]
    names(S) <- gsub(paste(visual,'.',sep=''),'',names(S))
    sens_wl <- sens[,'wl']
}else{
    S <- visual[,-which(names(visual)=='wl')]
    sens_wl <- visual[,'wl']
    fullS <- visual
    visual <- 'user-defined'
    }

conenumb <- dim(S)[2]

if(visual2 == 'segment')
  conenumb <- 'seg'

# transform from percentages to proportions according to Vorobyev 2003

if(max(y) > 1)
  y <- y/100

# check if wavelength range matches
  if(!isTRUE(all.equal(wl, sens_wl, check.attributes = FALSE)) & 
  !inherits(visual2, 'try-error'))
    stop('wavelength range in spectra and visual system data do not match - spectral 
         data must range between 300 and 700 nm in 1-nm intervals. Consider 
         interpolating using as.rspec().')

  if(!isTRUE(all.equal(wl, sens_wl, check.attributes = FALSE)))
    stop('wavelength range in spectra and visual system data do not match')

# DEFINING ILLUMINANT & BACKGROUND

bgil <- bgandilum

if(!inherits(illum2,'try-error')){
  illum <- bgil[,grep(illum2,names(bgil))]
  }else{
    illum2 <- 'user-defined'
    }

if(illum2=='ideal')
  illum <- rep(1,dim(rspecdata)[1])


if(!inherits(bg2,'try-error')){
  if(is.null(bkg)) stop('chosen background is NULL')
  bkg <- bgil[,grep(bg2,names(bgil))]
  }else{
    bg2 <- 'user-defined'
    }

if(bg2=='ideal')
  bkg <- rep(1,dim(rspecdata)[1])
  
# Defining ocular transmission

trdat <- transmissiondata

if(!inherits(tr2,'try-error')){
  if(is.null(trans)) stop('chosen transmission is NULL')
  trans <- trdat[,grep(tr2,names(trdat))]
  }else{
    tr2 <- 'user-defined'
    }

if(tr2=='ideal')
  trans <- rep(1,dim(rspecdata)[1])

if(tr2 != 'ideal' & visual == 'user-defined'){
	if('sensmod' %in% class(fullS))
		if(attr(fullS,'om'))
		  warning('The visual system being used appears to already incorporate ocular transmission. Using anything other than trans=',dQuote('ideal'),'means ocular media effects are being applied a second time.', call.=FALSE)
}


if('rspec' %in% class(bkg)){
  bkgwhichused <- names(bkg)[2]
  bkg <- bkg[,2]
  warning(paste('Background is an rspec object; first spectrum (', 
    dQuote(bkgwhichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
}

if('data.frame' %in% class(bkg) | 'matrix' %in% class(bkg) & 
  !'rspec' %in% class(bkg)){
  bkgwhichused <- names(bkg)[1]
  bkg <- bkg[,1]
  warning(paste('Background is a matrix or data frame; first column (', 
    dQuote(bkgwhichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
  }


# scale background from percentage to proportion
if(max(bkg) > 1)
  bkg <- bkg/100

# scale transmission from percentage to proportion
if(max(trans) > 1)
  trans <- trans/100
  
# is the illuminant a matrix, dataframe or rspec?

if('rspec' %in% class(illum)){
  whichused <- names(illum)[2]
  illum <- illum[,2]
  warning(paste('Illuminant is an rspec object; first spectrum (', 
    dQuote(whichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
}

if('data.frame' %in% class(illum) | 'matrix' %in% class(illum) & 
  !'rspec' %in% class(illum)){
  whichused <- names(illum)[1]
  illum <- illum[,1]
  warning(paste('Illuminant is a matrix or data frame; first column (', 
    dQuote(whichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
  }

# scale illuminant
illum <- illum * scale

indices <- 1:dim(S)[2]

# Filter specs by transmission

y <- y * trans

# calculate Qi
if(substr(visual2, 1, 3) == 'cie'){  # Slightly different for CIE
  K <- 100/colSums(S[2] * illum)
  Qi <- data.frame(sapply(indices, function(x) colSums(y * S[, x] * illum) * K))
}else if(visual == 'segment'){ # Slightly different for segment
  B <- apply(y, 2, sum)
  Qi <- data.frame(sapply(indices, function(x) colSums(y * S[, x] * illum) * B))
}else{
  Qi <- data.frame(sapply(indices, function(x) colSums(y * S[, x] * illum)))
}

# in case rspecdata only has one spectrum

if(dim(Qi)[2] < 2){
  Qi <- data.frame(t(Qi))
  rownames(Qi) <- names(y)
}

names(Qi) <- names(S)


# calculate achromatic contrast

# user-defined achromatic receptor

if(inherits(achromatic2,'try-error')){

  achromatic2 <- 'user-defined'

  # is achromatic a matrix, dataframe or rspec?

  if('rspec' %in% class(achromatic)){
    whichused <- names(achromatic)[2]
    achromatic <- achromatic[,2]
    warning(paste('Achromatic is an rspec object; first spectrum (', 
      dQuote(whichused),') has been used (remaining columns ignored)', sep='')
      , call.=FALSE)
  }

  if('data.frame' %in% class(achromatic) | 'matrix' %in% class(achromatic) & 
    !'rspec' %in% class(achromatic)){
    whichused <- names(achromatic)[1]
    achromatic <- achromatic[,1]
    warning(paste('Achromatic is a matrix or data frame; first column (', 
      dQuote(whichused),') has been used (remaining columns ignored)', sep='')
      , call.=FALSE)
    }

  L <- achromatic
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))

  }
  
# using one of the predefined receptors

if(achromatic2=='bt.dc' | achromatic2=='ch.dc' | achromatic2=='st.dc' | achromatic2=='md.r1'){
  L <- sens[,grep(achromatic2,names(sens))]
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='ml'){
  L <- rowSums(S[,c(dim(S)[2]-1,dim(S)[2])])
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='l'){
  L <- S[, dim(S)[2]]
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='all'){
  L <- rowSums(S)
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='segment'){
  Qi <- data.frame(cbind(Qi, B))
}

if(achromatic2 == 'none'){
	L   <- NULL
	lum <- NULL
}

# von Kries correction (constant adapting background)

vk <- "(von Kries color correction not applied)"

# quantum catch normalized to the background (qi = k*Qi)

if(vonkries){
  if(!is.null(lum))
    S <- data.frame(cbind(S,L))
  
  if(substr(visual, 1, 3) == 'cie'){
    k <- 1/(colSums(S * bkg * illum) * K)
    Qi <- data.frame(t(t(Qi) * k))
  }else{
    k <- 1/(colSums(S * bkg * illum))
    Qi <- data.frame(t(t(Qi) * k))
  }
    vk <- "(von Kries color correction applied)"
}

fi <- log(Qi)  # fechner law (signal ~ log quantum catch)
Ei <- Qi / (Qi + 1)  # hyperbolic transform

# make references for jnd2xyz
resrefs <- switch(qcatch,
  Qi = matrix(apply(Qi,2,min), nrow=dim(Qi)[2], ncol=dim(Qi)[2], byrow=TRUE),
  fi = matrix(apply(fi,2,min), nrow=dim(fi)[2], ncol=dim(fi)[2], byrow=TRUE),
  Ei = matrix(apply(fi,2,min), nrow=dim(fi)[2], ncol=dim(fi)[2], byrow=TRUE)
)

diag(resrefs) <- switch(qcatch,
  Qi = apply(Qi,2,max),
  fi = apply(fi,2,max),
  Ei = apply(Ei,2,max)
)

colnames(resrefs) <- colnames(Qi)
rownames(resrefs) <- paste0('refforjnd2xyz.',colnames(Qi))
resrefs <- rbind(resrefs, refforjnd2xyz.acent = 1)


matrix(apply(Qi,2,min), nrow=dim(Qi)[2], ncol=dim(Qi)[2], byrow=TRUE)

# Convert to relative
if(relative & !is.null(lum)){
  Qi[,-dim(Qi)[2]] <- Qi[,-dim(Qi)[2]]/rowSums(Qi[,-dim(Qi)[2]])
  fi[,-dim(fi)[2]] <- fi[,-dim(fi)[2]]/rowSums(fi[,-dim(fi)[2]])
  Ei[,-dim(Ei)[2]] <- Ei[,-dim(Ei)[2]]/rowSums(Ei[,-dim(Ei)[2]])
  
  resrefs[,-dim(resrefs)[2]] <- resrefs[,-dim(resrefs)[2]]/
    rowSums(resrefs[,-dim(resrefs)[2]])

# Place dark specs in achromatic center?
# blacks <- which(norm.B < 0.05) #find dark specs
# Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
}

if(relative & is.null(lum)){
    Qi <- Qi/rowSums(Qi)
    fi <- fi/rowSums(fi)
    Ei <- Ei/rowSums(Ei)
    # Place dark specs in achromatic center?
    # blacks <- which(norm.B < 0.05) #find dark specs
    # Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
    
    resrefs <- resrefs/rowSums(resrefs)
}

# OUTPUT

res <- switch(qcatch, Qi = Qi, fi = fi, Ei = Ei)

class(res) <- c('vismodel', 'data.frame')

# Descriptive attributes

attr(res, 'qcatch') <- qcatch
attr(res,'visualsystem.chromatic') <- visual
attr(res,'visualsystem.achromatic') <- achromatic2
attr(res,'illuminant') <- paste(illum2,', scale = ', scale, " ", vk, sep='')
attr(res,'background') <- bg2
attr(res,'transmission') <- tr2
attr(res,'relative') <- relative
attr(res, 'conenumb') <- conenumb # previously dim(S)[2], but that overestimated b/c of binding L to S
attr(res, 'vonkries') <- vonkries

# Data attributes
attr(res, 'data.visualsystem.chromatic') <- S
attr(res, 'data.visualsystem.achromatic') <- L
attr(res, 'data.illuminant') <- illum
attr(res, 'data.background') <- bkg
attr(res, 'data.transmission') <- trans
attr(res, 'resrefs') <- resrefs

res

}
