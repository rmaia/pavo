#' Model spectra in a colourspace
#'
#' \code{colorspace} Plots reflectance spectra in the appropriate colorspace
#' 
#' @param modeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with columns representing quantum catches).
#' 
#' @examples \dontrun{
#' data(flowers)
#' 
#' # Dichromat space
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colorspace(vis.flowers, space = 'di')
#' summary(di.flowers)
#'
#' # Colour hexagon 
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colorspace(vis.flowers, space = 'hexagon')
#' plot(hex.flowers)
#' 
#' # Maxwell triangle
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' maxwell.flowers <- colorspace(vis.flowers, space = 'maxwell')
#' plot(maxwell.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
#'    Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour science 
#'    using MATLAB. John Wiley & Sons.
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of 
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a 
#'    component of flower reflections, and the colour perception of Hymenoptera. 
#'    Vision research, 34(11), 1489-1508.
#' @references Troje N. (1993). Spectral categories in the learning behaviour
#'  of blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage 
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings. 
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns 
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision
#'    - behavioural tests and physiological concepts. Biological Reviews, 78,
#'    81 - 118.
#' @references Backhaus W. (1991). Color opponent coding in the visual system
#'  of the honeybee. Vision Research, 31, 1381-1397.

colorspace <- function(modeldata, space = c('di', 'maxwell', 'hexagon', 'tcs', 'coc', 'categorical', 'ciexyz', 'cielab')){
  
  space2 <- try(match.arg(space), silent = T)

  if(inherits(space2, 'try-error'))
    stop('No colorspace selected')

  if(space2 == 'di'){
    return(.dispace(modeldata))
  }
  
  if(space2 == 'maxwell'){
    return(.maxwell(modeldata))
  }
  
  if(space2 == 'hexagon'){
    return(.hexagon(modeldata))
  }
  
  if(space2 == 'tcs'){
    return(.tcs(modeldata))
  }
  
  if(space2 == 'coc'){
    return(.coc(modeldata))
  }
  
  if(space2 == 'categorical'){
    return(.categorical(modeldata))
  }
  
  if(space2 == 'ciexyz'){
    return(.cie(modeldata, 'XYZ'))
  }
  
  if(space2 == 'cielab'){
    return(.cie(modeldata, 'LAB'))
  }
  
}