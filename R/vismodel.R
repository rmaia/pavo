#' Visual Models
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate quantum 
#' catches at each photoreceptor. Relative values may also be obtained, in which
#' case the model reduces to the color space as described in Endler & Mielke (2005)
#' and Stoddard & Prum (2008).
#' 
#' @param specdata (required) Data frame containing reflectance spectra at each column.
#' must contain a \code{wl} column identifying the wavelengths for the reflectance values.
#' @param visual The visual system to be used. Currently implemented system are: 
#' \code{avg.uv}: average avian UV system; \code{avg.v}: average avian V system; 
#' \code{bt}: Blue tit \emph{Cyanistes caeruleus} (REF); \code{star}: Starling
#' \emph{Sturnus vulgaris} (REF); and \code{pfowl}: the peafowl 
#' \emph{Pavo cristatus} (REF).
#' @param relative Should relative quantum catches be returned (i.e. is it a color
#' space model? Defaults to \code{TRUE})
#' @param ilum A vector containing the iluminant. Must be the same length as the columns
#' in \code{specdata}. (Default assumes an idealized iluminant of 1)
#' @param bkg A vector containing the background. Must be the same length as the columns
#' in \code{specdata}. (Default assumes an idealized background of 1)
#' @return A list containing the following data frames:
#' @return \code{descriptive}: descriptive statistics of maximum and normalized 
#' reflectance, and wavelength of maximum reflectance (hue)
#' @return \code{Qi}: Quantum catch for each photoreceptor (which sum to 1 if 
#' \code{relative = TRUE})
#' @return \code{qi}: Quantum catch normalized to the adaptingbackground according 
#' to the von Kries transformation.
#' @return \code{fi}: Quantum catch according to Fechner law (the signal of the receptor
#' channel is proportional to the logarithm of the quantum catch)
#' @export
#' @examples \dontrun{
#' #INCLUDE EXAMPLE}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress In Retinal And Eye Research, 20(5), 675-703.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405–431.

vismodel<-function(specdata, visual=c("avg.uv", "avg.v", "bt", "star", "pfowl"), relative=TRUE, ilum=ideal, bkg=ideal)
{

# remove & save colum with wavelengths

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
y <- specdata[,-wl_index]

sens<- pavo::vissyst

# check if wavelength range matches

  if(!identical(wl,sens$wl))
    stop('wavelength in spectra table and visual system chosen do not match')

# get visual system to use

visual <- match.arg(visual)

S <- sens[,grep(visual,names(sens))]
names(S) <- gsub(paste(visual,'.',sep=''),'',names(S))

#DEFINING ILUMINANT & BACKGROUND
ideal <- rep(1,dim(specdata)[1])

# brightness
norm.B <- colSums(y)/(dim(y)[1]*100)
max.B <- apply(y,2,max)

# wavelength of maximum reflectance
lambdamax <- wl[max.col(t(y))]

descriptive <- data.frame(lambdamax,norm.B,max.B)

# scale to maximum reflectance = 1
yscale <- apply(y,2,function(x) x/max(x))

#SEM NORMALIZAR
#x<-y/100 #só transformando em proporção, ao invés de porcentabem (assim 100% reflectance = 1.0 )

#Qi
# at the moment this will only work for avian type visual systems. 
# could not figure out yet a way of automating without using loopso if more or
# les than the 4 columns of the avian system.

Qi <- matrix(NA,nrow=dim(y)[2], ncol=4)

Qi[,1] <- colSums(y*S[,1]*ilum)
Qi[,2] <- colSums(y*S[,2]*ilum)
Qi[,3] <- colSums(y*S[,3]*ilum)
Qi[,4] <- colSums(y*S[,4]*ilum)

Qi <- as.data.frame(Qi, row.names=names(y))
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
res
}