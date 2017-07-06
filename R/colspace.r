#' Model spectra in a colorspace
#'
#' Models reflectance spectra in a colorspace. For information on plotting arguments
#' and graphical parameters, see \code{\link{plot.colspace}}.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with columns representing quantum catches).
#' @param space Which colorspace/model to use. Options are:
#' \itemize{
#'    \item \code{auto}: if data is a result from \code{vismodel}, 
#'    applies \code{di}, \code{tri} or \code{tcs} if input visual model had two, three or four
#'    cones, respectively.
#'    \item \code{di}: dichromatic colourspace. See \code{\link{dispace}} for details.
#'    (\link[=diplot]{plotting arguments})
#'    \item \code{tri}: trichromatic colourspace (i.e. Maxwell triangle). See \code{\link{trispace}} for details. (\link[=triplot]{plotting arguments})
#'    \item \code{tcs}: tetrahedral colourspace. See \code{\link{tcspace}} for details.
#' (\link[=tetraplot]{plotting arguments})
#'    \item \code{hexagon}: the trichromatic colour-hexagon of Chittka (1992). See \code{\link{hexagon}} for details. (\link[=hexplot]{plotting arguments})
#'    \item \code{coc}: the trichromatic colour-opponent-coding model of Backhaus (1991). See \code{\link{coc}} for details. (\link[=cocplot]{plotting arguments})
#'    \item \code{categorical}: the tetrachromatic categorical fly-model of Troje (1993). See \code{\link{categorical}} for details. (\link[=catplot]{plotting arguments})
#'    \item \code{ciexyz}: CIEXYZ space. See \code{\link{cie}} for details. (\link[=cieplot]{plotting arguments})
#'    \item \code{cielab}: CIELAB space. See \code{\link{cie}} for details. (\link[=cieplot]{plotting arguments})
#'    \item \code{cielch}: CIELCh space. See \code{\link{cie}} for details. (\link[=cieplot]{plotting arguments})
#'    \item \code{segment}: segment analysis of Endler (1990). See \code{\link{segspace}} for details. (\link[=segplot]{plotting arguments})
#' }
#' 
#' @examples \dontrun{
#' data(flowers)
#' 
#' # Dichromat
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colspace(vis.flowers, space = 'di')
#'
#' # Colour hexagon 
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, 
#'                         vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colspace(vis.flowers, space = 'hexagon')
#' 
#' # Trichromat
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' tri.flowers <- colspace(vis.flowers, space = 'tri')
#' plot(tri.flowers)
#' 
#' # Tetrachromat
#' vis.flowers <- vismodel(flowers, visual = 'bluetit')
#' tcs.flowers <- colspace(vis.flowers, space = 'tcs')
#' 
#' # Categorical
#' vis.flowers <- vismodel(flowers, visual = 'musca', achro = 'md.r1')
#' cat.flowers <- colspace(vis.flowers, space = 'categorical')
#' }
#' 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
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
#' @references Endler, J. A. (1990) On the measurement and classification of 
#' color in studies of animal color patterns. Biological Journal of the Linnean 
#' Society, 41, 315-352.

colspace <- function(vismodeldata, 
                     space = c('auto', 'di', 'tri', 'tcs', 'hexagon', 'coc', 'categorical', 'ciexyz', 'cielab', 'cielch', 'segment'))
  {
  
  space2 <- try(match.arg(space), silent = T)

  if(inherits(space2, 'try-error'))
    stop('Invalid colorspace selected')
  
  # check if there is a resrefs attribute and bind it
  	if(!is.null(attr(vismodeldata, 'resrefs'))){
  		attribdat <- attributes(vismodeldata)
  		colsincommon <- intersect(colnames(vismodeldata), 
  		                          colnames(attr(vismodeldata, 'resrefs')))
  		
  		vismodeldata <- rbind(
  		             attr(vismodeldata, 'resrefs')[, colsincommon],
  		             vismodeldata[, colsincommon]
  		             )
  		attribdat$names <- attributes(vismodeldata)$names
  		attribdat$row.names <- attributes(vismodeldata)$row.names
  		attributes(vismodeldata) <- attribdat
  	}
  
  if(space2 == 'auto'){
  	res<- 
  	switch(as.character(attr(vismodeldata, 'conenumb')),
  	  '2' = dispace(vismodeldata),
  	  '3' = trispace(vismodeldata),
  	  '4' = tcspace(vismodeldata),
  	  'seg' = segspace(vismodeldata)
  	  )
  } else{
  	res <-
  	switch(space2,
  	'di' = dispace(vismodeldata),
  	'tri' = trispace(vismodeldata),
  	'hexagon' = hexagon(vismodeldata),
  	'tcs' = tcspace(vismodeldata),
  	'coc' = coc(vismodeldata),
  	'categorical' = categorical(vismodeldata),
  	'ciexyz' = cie(vismodeldata, 'XYZ'),
  	'cielab' = cie(vismodeldata, 'LAB'),
  	'cielch' = cie(vismodeldata, 'LCh'),
  	'segment' = segspace(vismodeldata)
  	)
  }
  
  attr(res, 'resrefs') <- NULL
  
  #remove reference results
  arethererefs <- grep('whiref|bluref|redref|greref', rownames(res))
  if(length(arethererefs) > 0){
    attribres <- attributes(res)

    resrefs <- res[arethererefs, ]
    res <- res[-arethererefs, ] 
    
    attribres$names <- attributes(res)$names
    attribres$row.names <- attributes(res)$row.names
    attributes(res) <- attribres

    attr(res, 'resrefs') <- resrefs
  }

res
}