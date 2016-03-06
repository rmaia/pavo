#' Plot Spectra in a Colourspace
#'
#' \code{plot.colspace} Plots reflectance spectra in the appropriate colorspace
#' 
#' @param clrspdata (required) an object of class \code{colspace}, the result of
#'  \code{colspace()}. Note that the below options will vary by selected \code{space} 
#' 
#' @param labels plot verticy labels (or category labels, when \code{space = \strong{'categorical'}})? 
#'  Defaults to \code{TRUE}
#' @param cex.labels character expansion factor for labels when \code{labels = TRUE})
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8)
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to grey)
#' @param out.lwd line width for plot outline, or the width of the monochromatic loci
#'  outline when \code{space = \strong{'ciexyz'}} if \code{mono = TRUE} (defaults to 1).  
#' @param out.lcol line colour for plot outline, or the colour of the monochromatic loci
#'  outline in \code{space = \strong{'ciexyz'}} if \code{mono = TRUE} (defaults to black). 
#' @param out.lty line type for plot outline, or the line type for the monochromatic loci
#'  outline in \code{space = \strong{'ciexyz'}} if \code{mono = TRUE} (defaults to 1).
#' @param tick.loc a numeric vector specifying the location of tick marks on x & y axes,
#'  only when \code{space = \strong{'di'}}
#' @param mono plot the monochromatic loci (the 'horseshoe') when \code{space = \strong{'ciexyz'}}? 
#'  Defaults to \code{TRUE}.   
#' @param sectors plot the bee-hue sector dividers when \code{space = \strong{'hexagon'}}? 
#' Options are:
#'    \itemize{ 
#'        \item \code{'none'}: No sectors (default)
#'        \item \code{'fine'}: 36 10-degree sectors
#'        \item \code{'coarse'}: six bee-hue sectors (blue, blue-green, green, uv-green, uv, uv-blue).
#'        }
#'        
#' @param col.sec line colour of bee-hue sector dividers when \code{space = \strong{'hexagon'}}. 
#'  Defaults to \code{'grey'}
#' @param new should a new 3D plot be called, when \code{space = \strong{'tcs'}} (defaults to \code{FALSE})?
#' @param hspin if \code{TRUE}, the graphic will spin horizontally (around the 'z' axis, when \code{space = \strong{'tcs'}})
#'  (defaults to \code{FALSE})
#' @param vspin if \code{TRUE}, the graphic will spin vertically (around the 'x' axis, when \code{space = \strong{'tcs'}})
#'  (defaults to \code{FALSE})
#' @param floor if \code{TRUE}, a reference xy plane is plotted under the tetrahedron, when \code{space = \strong{'tcs'}} 
#'  (defaults to \code{TRUE})
#' @param grid if \code{TRUE}, connects the polygon outlining the volume occupied by points, when \code{space = \strong{'tcs'}}
#'  (defaults to \code{TRUE})
#' @param grid.alpha transparecny of the volume polygon grid lines, when \code{space = \strong{'tcs'}}
#' @param fill if \code{TRUE}, fills the volume occupied by points, when \code{space = \strong{'tcs'}} 
#'  (WARNING: transparency is not saved properly if exported using \code{rgl.postscript})
#'  (defaults to \code{TRUE}).
#' @param ... additional graphical options. See \code{\link{par}} 
#' 
#' @return \code{plot.colspace} creates a 2D colspace plot appropriate to the input data.
#'  \code{points.colspace} adds points to the plot. When \code{space = \strong{'tcs'}}, 
#'  it creates an interactive 3D plot using functions of the package \code{rgl}, based on 
#'  openGL capabilities. It can be manipulated with the mouse (left button: rotate along 
#'  'z' axis; right button: rotate along 'x' axis; third button: zoom). \code{tcsvol} creates 
#'  polygon based on points, determining the volume occupied by them in the colorspace.
#' 
#' @examples \dontrun{
#' data(flowers)
#' 
#' # Dichromat
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colspace(vis.flowers, space = 'di')
#' plot(di.flowers)
#'
#' # Colour hexagon 
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colspace(vis.flowers, space = 'hexagon')
#' plot(hex.flowers)
#' 
#' # Tetrahedron
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' plot(tcs.sicalis, size = 0.005)
#' rgl.postscript('testplot.pdf',fmt = 'pdf') 
#' rgl.snapshot('testplot.png')
#'
#' ## Add points to tetrahedron
#' patch <- rep(c('C','T','B'), 7)
#' tcs.crown <- subset(tcs.sicalis, 'C') #### FIX SUBSET ###
#' tcs.breast <- subset(tcs.sicalis, 'B') 
#' plot(tcs.crown, col ='blue')
#' points(tcs.breast, col ='red')
#'
#' ## Plot convex hull in tetrahedron
#' plot(tcs.sicalis, col = 'blue', size = 0.005)
#' vol(tcs.sicalis)
#' }
#' 
#' @seealso \code{\link{plot}}
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

plot.colspace <- function(clrspdata, ...){
  
  if(attr(clrspdata, 'clrsp') == 'hexagon'){
    .hexplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'coc'){
    .cocplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'categorical'){
    .catplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'dispace'){
    .diplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'trispace'){
    .triplot(clrspdata, ...)
  }
  
  if(grepl('CIE', attr(clrspdata, 'clrsp'))){
    .cieplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'tcs'){
    .tcsplot(clrspdata, ...)
  }
  
}