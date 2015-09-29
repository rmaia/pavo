
#' Visual Models
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate quantum 
#' catches at each photoreceptor. Relative values may also be obtained, in which
#' case the model reduces to the color space as described in Endler & Mielke (2005)
#' and Stoddard & Prum (2008).
#' 
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec}
#' that has wavelength range in the first column, named 'wl', and spectral measurements in the 
#' remaining columns. 
#' @param qcatch Which quantal catch metric to return. Options are:
#' \itemize{
#' \item \code{Qi}: Quantum catch for each photoreceptor 
#' \item \code{fi}: Quantum catch according to Fechner law (the signal of the receptor
#' channel is proportional to the logarithm of the quantum catch)
#' }
#' @param visual the visual system to be used. Options are:
#' \itemize{
#'	\item a data frame such as one produced containing by \code{sensmodel}, containing 
#' sensitivity for the user-defined visual system. The data frame must contain a \code{'wl'}
#' column with the range of wavelengths included, and the sensitivity for each other 
#' cone as a column
#' \item \code{avg.uv}: average avian UV system
#' \item \code{avg.v}: average avian V system
#' \item \code{bt}: Blue tit \emph{Cyanistes caeruleus} visual system
#' \item \code{star}: Starling \emph{Sturnus vulgaris} visual system  
#' \item \code{pfowl}: Peafowl \emph{Pavo cristatus} visual system
#' \item \code{apis}: Honeybee \emph{Apis mellifera} visual system
#' \item \code{cie1931}: Human CIE 1931 color matching functions
#' }
#' @param achromatic the sensitivity data to be used to calculate luminance (achromatic)
#' cone stimulation. Either a vector containing the sensitivity for a single receptor, 
#' or one of the options: 
#' \itemize{
#'	\item \code{bt.dc}: Blue tit \emph{Cyanistes caeruleus} double cone
#'  \item \code{ch.dc}: Chicken \emph{Gallus gallus} double cone
#'  \item \code{st.dc}: Starling \emph{Sturnus vulgaris} double cone
#'  \item \code{ml}: sum of the two longest-wavelength cones
#'  \item \code{none}
#' }
#' @param illum either a vector containing the illuminant, or one of the options:
#' \itemize{ 
#' \item \code{ideal}: homogeneous illuminance of 1 accross wavelengths (default)
#' \item \code{'bluesky'}
#' \item \code{'D65'}: standard daylight
#' \item \code{'forestshade'}
#' }
#' @param bkg either a vector containing the background spectra, or an ideal (white) 
#' background is used (Default assumes an idealized homogeneous background).
#' @param relative should relative quantum catches be returned (i.e. is it a color
#' space model? Defaults to \code{TRUE}).
#' @param vonkries logical. Should the von Kries color correction transformation be applied?
#' (defaults to \code{FALSE})
#' @param scale a value by which the illuminant will be multiplied. Useful for when the 
#' illuminant is a relative value (i.e. transformed to a maximum of 1 or to a percentage),
#' and does not correspond to quantum flux units ($umol*s^-1*m^-2$). Useful values
#' are, for example, 500 (for dim light) and 10000 (for bright illumination). Note that if
#' \code{vonkries=TRUE} this transformation has no effect.
#'
#' @return An object of class \code{vismodel} containing the photon catches for each of the 
#' photoreceptors considered. Information on the parameters used in the calculation are also
#' stored and can be called using the \code{summary.vismodel} function.
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis)}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress In Retinal And Eye Research, 20(5), 675-703.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

vismodel <- function(rspecdata, qcatch = c('Qi','fi'),
  visual = c("avg.uv", "avg.v", "bt", "star", "pfowl", "apis", "cie1931"), 
  achromatic = c("none","bt.dc","ch.dc", 'st.dc',"ml"),
  illum = c('ideal','bluesky','D65','forestshade'), 
  vonkries=FALSE, scale=1, bkg = 'ideal', relative=TRUE)
{

# remove & save colum with wavelengths

wl_index <- which(names(rspecdata)=='wl')
wl <- rspecdata[,wl_index]
y <- rspecdata[,-wl_index]

# in case rspecdata only has one spectrum

if(is.null(dim(y))){
  y <- data.frame(rspecdata[,-wl_index])
  names(y) <- names(rspecdata)[-wl_index]
  }

visual2 <- try(match.arg(visual), silent=T)
sens <- vissyst

if(!inherits(visual2,'try-error')){
  
  visual <- match.arg(visual)
  S <- sens[,grep(visual,names(sens))]
  names(S) <- gsub(paste(visual,'.',sep=''),'',names(S))
  sens_wl <- sens[,'wl']
  
}else{
    S <- visual[,-which(names(visual)=='wl')]
    sens_wl <- visual[,'wl']
    visual <- 'user-defined'
    }

# transform from percentages to proportions according to Vorobyev 2003

if(max(y) > 1)
  y <- y/100

# check if wavelength range matches
  if(!isTRUE(all.equal(wl,sens_wl, check.attributes=FALSE)) & 
  !inherits(visual2,'try-error'))
    stop('wavelength range in spectra and visual system data do not match - spectral data must range between 300 and 700 nm in 1-nm intervals. Consider interpolating using as.rspec().')

  if(!isTRUE(all.equal(wl,sens_wl, check.attributes=FALSE)))
    stop('wavelength range in spectra and visual system data do not match')


#DEFINING ILLUMINANT & BACKGROUND

bgil<- bgandilum

illum2 <- try(match.arg(illum), silent=T)
if(!inherits(illum2,'try-error')){
  illum <- bgil[,grep(illum2,names(bgil))]
  }else{
    illum2 <- 'user-defined'
    }

if(illum2=='ideal')
  illum <- rep(1,dim(rspecdata)[1])

bg2 <- try(match.arg(bkg), silent=T)
if(!inherits(bg2,'try-error')){
  bkg <- bgil[,grep(bg2,names(bgil))]
  }else{
    bg2 <- 'user-defined'
    }

if(bg2=='ideal')
  bkg <- rep(1,dim(rspecdata)[1])

# scale background from percentage to proportion
if(max(bkg) > 1)
  bkg <- bkg/100
  
# is the illuminant a matrix, dataframe or rspec?

if('rspec' %in% class(illum)){
  whichused <- names(illum)[2]
  illum <- illum[,2]
  warning(paste('Illuminant is an rspec object; first spectrum (', 
    dQuote(whichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
}

if( 'data.frame' %in% class(illum) | 'matrix' %in% class(illum) & 
  !'rspec' %in% class(illum)){
  whichused <- names(illum)[1]
  illum <- illum[,1]
  warning(paste('Illuminant is a matrix or data frame; first column (', 
    dQuote(whichused),') has been used (remaining columns ignored)', sep='')
    , call.=FALSE)
  }


# scale illuminant
illum <- illum * scale

indices = 1:dim(S)[2]

# calculate Qi

Qi <- data.frame(sapply(indices, function(x) colSums(y*S[,x]*illum)))

# in case rspecdata only has one spectrum

if(dim(Qi)[2] < 2){
  Qi <- data.frame(t(Qi))
  rownames(Qi) <- names(y)
}

names(Qi) <- names(S)


# calculate achromatic contrast

achromatic2 <- try(match.arg(achromatic), silent=T)

# user-defined achromatic receptor

if(inherits(achromatic2,'try-error')){

  achromatic2 <- 'user-defined'

  # is achromatic a matrix, dataframe or rspec?

  if('rspec' %in% class(achromatic)){
    whichused <- names(achromatic)[2]
    achromatic <- achromatic[,2]
    warning(paste('achromatic is an rspec object; first spectrum (', 
      dQuote(whichused),') has been used (remaining columns ignored)', sep='')
      , call.=FALSE)
  }

  if( 'data.frame' %in% class(achromatic) | 'matrix' %in% class(achromatic) & 
    !'rspec' %in% class(achromatic)){
    whichused <- names(achromatic)[1]
    achromatic <- achromatic[,1]
    warning(paste('achromatic is a matrix or data frame; first column (', 
      dQuote(whichused),') has been used (remaining columns ignored)', sep='')
      , call.=FALSE)
    }

  L <- achromatic
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))


  }
  
# using one of the predefined receptors

if(achromatic2=='bt.dc' | achromatic2=='ch.dc' | achromatic2=='st.dc'){
   L <- sens[,grep(achromatic2,names(sens))]
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='ml'){
   L <- rowSums(S[,c(dim(S)[2]-1,dim(S)[2])])
  lum <- colSums(y*L*illum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(achromatic2=='none'){
	L   <- NULL
	lum <- NULL
}

#qi 
# von Kries correction (constant adapting background)

vk <- "(von Kries color correction not applied)"

# quantum catch normalized to the background (qi = k*Qi)

if(vonkries){
  if(!is.null(lum))
    S <- data.frame(cbind(S,L))

  k <- 1/colSums(S*bkg*illum)

  Qi <- data.frame(t(t(Qi)*k))

  vk <- "(von Kries color correction applied)"
}
# fechner law (signal ~ log quantum catch)

fi <- log(Qi)


if(relative & !is.null(lum)){
  Qi[,-dim(Qi)[2]] <- Qi[,-dim(Qi)[2]]/rowSums(Qi[,-dim(Qi)[2]])
  fi[,-dim(fi)[2]] <- fi[,-dim(fi)[2]]/rowSums(fi[,-dim(fi)[2]])

# Place dark specs in achromatic center?
# blacks <- which(norm.B < 0.05) #find dark specs
# Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
}

if(relative & is.null(lum)){
  Qi <- Qi/rowSums(Qi)
  fi <- fi/rowSums(fi)

# Place dark specs in achromatic center?
# blacks <- which(norm.B < 0.05) #find dark specs
# Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
}

# OUTPUT
#res<-list(descriptive=descriptive,Qi=Qi, qi=qi, fi=fi)

qcatch <- match.arg(qcatch)

res <- switch(qcatch, Qi = Qi, fi = fi)

class(res) <- c('vismodel', 'data.frame')

# Descriptive attributes

attr(res, 'qcatch') <- qcatch
attr(res,'visualsystem') <- paste('chromatic: ', visual, ', achromatic: ',achromatic2, sep='')
attr(res,'illuminant') <- paste(illum2,', scale = ',scale," ",vk, sep='')
attr(res,'background') <- bg2
attr(res,'relative') <- relative
attr(res, 'conenumb') <- dim(S)[2]
attr(res, 'vonkries') <- vonkries

# Data attributes
attr(res, 'data.visualsystem.chromatic') <- S
attr(res, 'data.visualsystem.achromatic') <- L
attr(res, 'data.background') <- bkg


res
}
