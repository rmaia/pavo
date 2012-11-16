#' Visual Models
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate quantum 
#' catches at each photoreceptor. Relative values may also be obtained, in which
#' case the model reduces to the color space as described in Endler & Mielke (2005)
#' and Stoddard & Prum (2008).
#' 
#' @param specdata (required) Data frame containing reflectance spectra at each column.
#' Must contain a \code{wl} column identifying the wavelengths for the reflectance values.
#' @param sens Data frame, such as one produced by \code{sensmodel} containing
#' sensitivity for the user-defined visual system. The data frame must contain
#' a 'wl' column with the range of wavelengths included, and the
#' sensitivity for each other cone as a column. If not specified, the visual system will
#' be chosen according to one of the choices from the argument \code{visual}.
#' @param visual The visual system to be used. Currently implemented system are: 
#' \code{avg.uv}: average avian UV system; \code{avg.v}: average avian V system; 
#' \code{bt}: Blue tit \emph{Cyanistes caeruleus} (REF); \code{star}: Starling
#' \emph{Sturnus vulgaris} (REF); and \code{pfowl}: the peafowl 
#' \emph{Pavo cristatus} (REF).
#' @param luminance The sensitivity data to be used to calculate luminance (achromatic)
#' cone stimulation. Currently implemented system are: \code{bt.dc}: Blue tit 
#' \emph{Cyanistes caeruleus} double cone, \code{ch.dc}: Chicken \emph{Gallus gallus}
#' double cone, \code{ml}: sum of the longest-wavelength cones, or \code{none}
#' @param relative Should relative quantum catches be returned (i.e. is it a color
#' space model? Defaults to \code{TRUE})
#' @param ilum either a vector containing the illuminant, or one of the options: 
#' 'ideal' (total homogeneous illuminance accross wavelengths), 'bluesky', 'D65' (daylight),
#' or 'forestshade' (Default assumes an idealized illuminant of 1)
#' @param bkg either a vector containing the background spectra, or an ideal (white) 
#' background is used.
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
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

#ToDo: fix log adjustment when relative=T

vismodel <- function(specdata, sens=NULL, relative=TRUE, 
  visual = c("avg.uv", "avg.v", "bt", "star", "pfowl"), 
  luminance = c("bt.dc","ch.dc","ml","none"),
  ilum = c('ideal','bluesky','D65','forestshade'), bkg = 'ideal')
{

# remove & save colum with wavelengths

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
y <- specdata[,-wl_index]

# get visual system to use

if(is.null(sens)){
  sens <- pavo::vissyst

  visual <- match.arg(visual)

  S <- sens[,grep(visual,names(sens))]
  names(S) <- gsub(paste(visual,'.',sep=''),'',names(S))
  }else{
    S <- sens[,-which(names(sens)=='wl')]
    visual <- 'user-defined'
    sens <- pavo::vissyst
    }

sens_wl <- sens[,'wl']

# if relative=F, convert to proportions

# if(!relative)
  y <- y/100

# check if wavelength range matches
  if(!isTRUE(all.equal(wl,sens_wl, check.attributes=FALSE)))
    stop('wavelength in spectra table and visual system chosen do not match')


#DEFINING ILUMINANT & BACKGROUND

bgil<- pavo::bgandilum

ilum2 <- try(match.arg(ilum), silent=T)
if(!inherits(ilum2,'try-error')){
  ilum <- bgil[,grep(ilum2,names(bgil))]
  }else{
    ilum2 <- 'user-defined'
    }

if(ilum2=='ideal')
  ilum <- rep(1,dim(specdata)[1])

bg2 <- try(match.arg(bkg), silent=T)
if(!inherits(bg2,'try-error')){
  bkg <- bgil[,grep(bg2,names(bgil))]
  }else{
    bg2 <- 'user-defined'
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


# calculate luminance contrast

luminance <- match.arg(luminance)

if(luminance=='bt.dc' | luminance=='ch.dc'){
   L <- sens[,grep(luminance,names(sens))]
  lum <- colSums(y*L*ilum)
  Qi <- data.frame(cbind(Qi,lum))
}

if(luminance=='ml'){
   L <- rowSums(S[,c(dim(S)[2]-1,dim(S)[2])])
  lum <- colSums(y*L*ilum)
  Qi <- data.frame(cbind(Qi,lum))
}


#qi 
# von Kries correction (constant adapting background)

if(!is.null(lum))
  S <- data.frame(cbind(S,L))

k <- 1/colSums(S*bkg*ilum)

# quantum catch normalized to the background (qi = k*Qi)

qi <- t(t(Qi)*k)

# fechner law (signal ~ log quantum catch)

fi <- log(qi)


if(relative){
  Qi[,-dim(Qi)[2]] <- Qi[,-dim(Qi)[2]]/rowSums(Qi[,-dim(Qi)[2]])
  qi[,-dim(qi)[2]] <- qi[,-dim(qi)[2]]/rowSums(qi[,-dim(qi)[2]])
  fi[,-dim(fi)[2]] <- 1/fi[,-dim(fi)[2]]/rowSums(1/fi[,-dim(fi)[2]])

# Place dark specs in achromatic center?
# blacks <- which(norm.B < 0.05) #find dark specs
# Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
}



#OUTPUT
res<-list(descriptive=descriptive,Qi=Qi, qi=qi, fi=fi)
class(res) <- 'vismodel'
attr(res,'visualsystem') <- c(visual,luminance)
attr(res,'iluminant') <- ilum2
attr(res,'background') <- bg2
attr(res,'relative') <- relative
res
}