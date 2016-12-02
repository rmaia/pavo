#' Modelling spectral sensitivity
#'
#' Models spectral sensitivity (with oil droplets; optional) based on peak cone sensitivity
#' according to the models of Govardovskii et al. (2000) and Hart & Vorobyev (2005).
#'
#' @param peaksense (required) a vector with peak sensitivities for the cones to
#' model
#' @param range a vector of length 2 for the range over which to calculate the spectral 
#' sensitivities (defaults to 300nm to 700nm)
#' @param lambdacut a vector of same length as peaksense that lists the cut-off wavelength
#' value for oil droplets. Needs either \code{Bmid} or \code{oiltype} to also be entered.
#' See Hart and Vorobyev (2005)
#' @param Bmid a vector of same length as peaksense that lists the gradient of line 
#' tangent to the absorbance spectrum of the oil droplets. See Hart and Vorobyev (2005) 
#' @param oiltype a list of same length as peaksense that lists the oil droplet types
#' (currently accepts only "T", C", "Y", "R", "P") when Bmid is not known. Calculates
#' Bmid based on the regression equations found in Hart ad Vorobyev (2005).
#' @param beta logical. If \code{TRUE} the sensitivities will include the beta peak
#' See Govardovskii et al.(2000) (defaults to \code{TRUE}).
#' @param om a vector of same length as \code{range1}-\code{range2} that contains ocular media transmission data.
#' If included, cone sensitivity will be corrected for ocular media transmission. Currently accepts "bird" using 
#' values from Hart et al. (2005), or user-defined values.
#' @param integrate logical. If \code{TRUE}, each curve is transformed to have a total area
#' under the curve of 1 (best for visual models; defaults to \code{TRUE}).
#' @return A data frame of class \code{rspec} containing each cone model as a column.
#' @export
#' @examples \dontrun{
#' # Blue tit visual system based on Hart et al (2000)
#' bluesens <- sensmodel(c(371,448,502,563),beta=F,oiltype = c("T", "C", "Y","R"),om= TRUE)
#' # Danio aequipinnatus based on Govardovskii et al. (2000)
#' daniosens <- sensmodel(c(357,411,477,569))}
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}, Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references Govardovskii VI, Fyhrquist N, Reuter T, Kuzmin DG and Donner K. 2000. In search of the visual pigment template. Visual Neuroscience 17:509-528
#' @references Hart NS, and Vorobyev M. 2005. Modelling oil droplet absorption
#' spectra and spectral sensitivities of bird cone photoreceptors. Journal of
#' Comparative Physiology A. 191: 381-392
#' @references Hart NS, Partridge JC, Cuthill IC, Bennett AT (2000) Visual pigments, oil droplets, ocular media and cone photoreceptor
#' distribution in two species of passerine bird: the blue tit (Parus caeruleus L) and the blackbird (Turdus merula L). J Comp
#' Physiol A 186:375-387



sensmodel <- function(peaksense, range = c(300,700), lambdacut = NULL, Bmid = NULL, 
		             oiltype = NULL, beta = TRUE, om = NULL, integrate = TRUE) {

if (!is.null(lambdacut)){
 if (is.null(Bmid) & is.null(oiltype)) stop ("Bmid or oiltype must be included when including a lambdacut vector", call.=FALSE)
 if (length(lambdacut) != length(peaksense)) stop ("lambdacut must be same length as peaksense", call.=FALSE)
}  

sensecurves <- matrix(ncol = length(peaksense)+1,nrow = (range[2]-range[1]+1))
sensecurves[,1] <- c(range[1]:range[2])


#Sensitivities w/o oil droplets
for (i in 1: length(peaksense)){

peak <- 1/(exp(69.7*(.8795+.0459*exp(-(peaksense[i]-range[1])^2/11940)-(peaksense[i]/(range[1]:range[2]))))
  +exp(28*(.922-peaksense[i]/(range[1]:range[2])))+exp(-14.9*(1.104-(peaksense[i]/(range[1]:range[2]))))+.674)

betaband <- 0.26*exp(-(((range[1]:range[2])
  -(189+0.315*peaksense[i]))/(-40.5+0.195*peaksense[i]))^2)

if (beta==TRUE)peak <- peak + betaband
peak <- peak/max(peak)

if (!is.null(lambdacut)& !is.null(Bmid)){
 
 if (length(lambdacut) != length(Bmid)) stop ("lambdacut and Bmid must be of same length")

T.oil <- exp(-exp(-2.89*Bmid[i]*(range[1]:range[2]-lambdacut[i])+1.08))
peak <- peak*T.oil
}

if (!is.null(lambdacut) & !is.null(oiltype)){

  if (length(lambdacut) != length(oiltype)) stop ("lambdacut and oiltype must be of same length")

  if (oiltype[i] == "C") oil <- c(0.99, 24.38)
  if (oiltype[i] == "Y") oil <- c(0.9, 70.03)
  if (oiltype[i] == "R") oil <- c(0.99, 28.65)
  if (oiltype[i] == "P") oil <- c(0.96, 33.57)

# Oil droplet transmission from Hart and Vorobyev (2005)
if (oiltype[i] != "T"){
  T.oil <- exp(-exp(-2.89*(.5/((oil[1]*lambdacut[i]+oil[2])-lambdacut[i]))*(range[1]:range[2]-lambdacut[i])+1.08))
}
  if(oiltype[i] == "T") T.oil <- rep(1,range[2]-range[1])
  
peak <- peak*T.oil
}
# Apply ocular media transmission correction

if (!is.null(om)){
  if (length(om) == 1){
    if (om == "bird"){
    T.e <- log(8.928*10^-13*(range[1]:range[2])^5-2.595*10^-9*(range[1]:range[2])^4+3.006*10^-6*(range[1]:range[2])^3-.001736*(range[1]:range[2])^2+.5013*(range[1]:range[2])-55.56)
    T.e[which(T.e < 0)] <- 0
    peak <- peak * T.e}
  }
  else {
    T.e <- om
    peak <- peak * T.e
  }
}


sensecurves[, (i+1)] <- peak
  }

if(integrate==TRUE){
  sensecurves <- apply(sensecurves, 2, function(z) z/sum(z))
  sensecurves[,1] <- c(range[1]:range[2])
}

sensecurves <- data.frame(sensecurves)
names(sensecurves) <- c('wl',paste('lmax',peaksense,sep=''))
#sensecurves <- as.rspec(sensecurves)
class(sensecurves) <- c('rspec', 'data.frame')

sensecurves
}



