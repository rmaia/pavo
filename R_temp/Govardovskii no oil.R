#GOVARDOVSKII (2000) VISUAL TEMPLATE
#
#Modified from Eliason C by PPB
#' @param peaksense (required) A vector with peak sensitivities for the cones to
#' model
#' @param range(required) Wavelength range over which to calculate the spectral 
#' sensitivities
#' 


govardovskii <- function(peaksense,range) {

sensecurves <- matrix(ncol = length(peaksense)+1,nrow = (range[2]-range[1]+1))
sensecurves[,1] <- c(range[1]:range[2])

#Sensitivities w/o oil droplets
for (i in 1: length(peaksense)){

peak <- 1/(exp(69.7*(.8795+.0459*exp(-(peaksense[i]-range[1])^2/11940)-(peaksense[i]/(range[1]:range[2]))))
  +exp(28*(.922-peaksense[i]/(range[1]:range[2])))+exp(-14.9*(1.104-(peaksense[i]/(range[1]:range[2]))))+.674)

betaband <- 0.26*exp(-(((range[1]:range[2])
  -(189+0.315*peaksense[i]))/(-40.5+0.195*peaksense[i]))^2)

peak <- peak + betaband
peak <- peak/max(peak)

sensecurves[, (i+1)] <- peak
  }
sensecurves
}
