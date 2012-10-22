#' Visual Models
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate quantum 
#' catches at each photoreceptor. Relative values may also be obtained, in which
#' case the model reduces to the color space as described in Endler & Mielke (2005)
#' and Stoddard & Prum (2008).
#' 
#' @param specdata (required) Data frame containing reflectance spectra at each column.
#' must contain a \code{wl} column identifying the wavelengths for the reflectance values.
#' @param sens Data frame containing sensitivity for the user-defined visual system. The
#' data frame must contain a 'wl' column with the range of wavelengths included, and the
#' sensitivity for each other cone as a column. If not specified, the visual system will
#' be chosen according to one of the choices from the argument \code{visual}.
#' @param visual The visual system to be used. Currently implemented system are: 
#' \code{avg.uv}: average avian UV system; \code{avg.v}: average avian V system; 
#' \code{bt}: Blue tit \emph{Cyanistes caeruleus} (REF); \code{star}: Starling
#' \emph{Sturnus vulgaris} (REF); and \code{pfowl}: the peafowl 
#' \emph{Pavo cristatus} (REF).
#' @param relative Should relative quantum catches be returned (i.e. is it a color
#' space model? Defaults to \code{TRUE})
#' @param ilum either a vector containing the iluminant, or one of the options: 
#' 'ideal' (total homogeneous iluminance accross wavelengths), 'bluesky', 'd65' (daylight),
#' or 'forestshade' (Default assumes an idealized iluminant of 1)
#' @param bkg either a vector containing the background, or an ideal background is used.
#' in \code{specdata}. (Default assumes an idealized background of 1)
#' @return A list containing the following data frames:
#' @return \code{descriptive}: descriptive statistics of maximum and normalized 
#' reflectance, and wavelength of maximum reflectance (hue)
#' @return \code{Qi}: Quantum catch for each photoreceptor (which sum to 1 if 
#' \code{relative = TRUE})
#' @return \code{qi}: Quantum catch normalized to the adapting background according 
#' to the von Kries transformation.
#' @return \code{fi}: Quantum catch according to Fechner law (the signal of the receptor
#' channel is proportional to the logarithm of the quantum catch)
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis, by=rep(c('C','T','B'),7))}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress In Retinal And Eye Research, 20(5), 675-703.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405–431.

vismodel <- function(specdata, sens=NULL, 
  visual=c("avg.uv", "avg.v", "bt", "star", "pfowl"), relative=TRUE, ilum=c('ideal','bluesky','D65','forestshade'), bkg='ideal')
{

# remove & save colum with wavelengths

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
y <- specdata[,-wl_index]

# get visual system to use

if(is.null(sens)){
  sens<- pavo::vissyst

  visual <- match.arg(visual)

  S <- sens[,grep(visual,names(sens))]
  names(S) <- gsub(paste(visual,'.',sep=''),'',names(S))
  }else{
    S <- sens[,-which(names(sens)=='wl')]
    visual <- 'user-defined'
    }

sens_wl <- sens[,'wl']

# if relative=F, convert to proportions

if(!relative)
  y <- y/100

# check if wavelength range matches
  if(!isTRUE(all.equal(wl,sens_wl, check.attributes=FALSE)))
    stop('wavelength in spectra table and visual system chosen do not match')


#DEFINING ILUMINANT & BACKGROUND

bgil<- pavo::bgandilum

ilum2 <- try(match.arg(ilum), silent=T)
if(!inherits(ilum2,'try-error')){
  ilum <- bgil[,grep(ilum2,names(bgil))]	
  }

if(ilum2=='ideal')
  ilum <- rep(1,dim(specdata)[1])

bg2 <- try(match.arg(bkg), silent=T)
if(!inherits(bg2,'try-error')){
  bkg <- bgil[,grep(bg2,names(bgil))]	
  }

if(bg2=='ideal')
  bkg <- rep(1,dim(specdata)[1])


# brightness
norm.B <- colSums(y)/(dim(y)[1]*100)
max.B <- apply(y,2,max)

# wavelength of maximum reflectance
lambdamax <- wl[max.col(t(y))]

descriptive <- data.frame(lambdamax,norm.B,max.B)

# scale to maximum reflectance = 1
yscale <- apply(y,2,function(x) x/max(x))

#Qi
# at the moment this will only work for avian type visual systems. 
# UPDATE: can now handle different lengths of sensory systems.

indices = 1:dim(S)[2]

Qi <- data.frame(sapply(indices, function(x) colSums(y*S[,x]*ilum)))
names(Qi) <- names(S)

if(relative){
  Qi <- Qi/rowSums(Qi)

# Place dark specs in achromatic center?
# blacks <- which(norm.B < 0.05) #find dark specs
# Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
}

#qi 
# von Kries correction (constant adapting background)

k <- 1/colSums(S*bkg*ilum)

# quantum catch normalized to the background (qi = k*Qi)

qi <- t(t(Qi)*k)

# fechner law (signal ~ log quantum catch)

fi <- log(qi)


#OUTPUT
res<-list(descriptive=descriptive,Qi=Qi, qi=qi, fi=fi)
class(res) <- 'vismodel'
attr(res,'visualsystem') <-visual
attr(res,'relative') <- relative
res
}