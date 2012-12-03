#' Plot a Tetrahedral Color Space
#'
#' \code{ttplot} produces a 3D plot of a tetrahedral color space using OpenGL capabilities
#'
#' @param tcsdata (required) a data frame, possibly a result from the \code{tcs} 
#' function, containing values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param size size of the points in the plot (defaults to 0.02)
#' @param col color of the points in the plot (defaults to black)
#' @param new should a new 3D plot be called (defaults to \code{FALSE})?
#' @param hspin if \code{TRUE}, the graphic will spin horizontally (around the 'z' axis)(defaults to \code{FALSE}).
#' @param vspin if \code{TRUE}, the graphic will spin vertically (around the 'x' axis)(defaults to \code{FALSE}).
#' @param floor if \code{TRUE}, a reference xy plane is plotted under the tetrahedron (defaults to \code{TRUE}).
#' @param grid if \code{TRUE}, connects the polygon outlining the volume occupied by points (defaults to \code{TRUE}).
#' @param fill if \code{TRUE}, fills the volume occupied by points (WARNING: transparency
#' is not saved properly if exported using \code{rgl.postscript})(defaults to \code{TRUE}).
#' @return \code{ttplot} creates a 3D plot using functions of the package \code{rgl}, 
#' based on openGL capabilities. Plot is interactive and can be manipulated with the mouse 
#' (left button: rotate along 'z' axis; right button: rotate along 'x' axis; 
#' third button: zoom). \code{ttvol} creates polygon based on points, determining the volume
#' occupied by them in the colorspace. \code{ttpoints} adds points to the plot. Points are
#' currently plotted only as spheres to maintain export capabilities.
#'
#' @export
#'
#' @examples \dontrun{
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis)
#' ttplot(tcs.sicalis, size=0.005)
#' rgl.postscript('testplot.pdf',fmt='pdf') 
#' rgl.snapshot('testplot.png')
#'
#' # For adding points
#' patch <- rep(c('C','T','B'),7)
#' tcs.crown <- tcs.sicalis[patch=='C', ]
#' tcs.breast <- tcs.sicalis[patch=='B', ]
#' ttplot(tcs.crown, col='blue')
#' ttpoints(tcs.breast, col='red')
#'
#' # For plotting convex hull
#' ttplot(tcs.sicalis, col='blue', size=.005)
#' ttvol(tcs.sicalis)}
#' 
#' @seealso \code{\link{spheres3d}},\code{\link{rgl.postscript}}, 
#' \code{\link{rgl.snapshot}},\code{\link{rgl.par3d}} 
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

#ToDo: Add option to not plot tetrahedron

ttplot<- function(tcsdata, size=0.02, col='black', new=FALSE, hspin=FALSE, 
                  vspin=FALSE, floor=TRUE, grid=TRUE, fill=TRUE) {

# if(class(tcsdata)=='tcs'){
  # dat <- tcsdata$tcs	
  # }else{
    # dat <- tcsdata
    # }

if(new)
   open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))

# can't figure out how to change the character type

ttv=pavo::ttvertex

cu=t(col2rgb('#984EA3'))/255
cs=t(col2rgb('#377EB8'))/255
cm=t(col2rgb('#4DAF4A'))/255
cl=t(col2rgb('#E41A1C'))/255

plot3d(unlist(ttv[c('xu','xs','xm','xl')]),
		unlist(ttv[c('yu','ys','ym','yl')]),
		unlist(ttv[c('zu','zs','zm','zl')]), type='s', lit=F,
		radius=0.02, box=F, axes=F, xlab='',ylab='',zlab='',
		col=c(rgb(cu[1],cu[2],cu[3]), rgb(cs[1],cs[2],cs[3]), 
		rgb(cm[1],cm[2],cm[3]), rgb(cl[1],cl[2],cl[3])))


segments3d(ttv[c('xu','xs')], ttv[c('yu','ys')], ttv[c('zu','zs')], color='lightgrey')
segments3d(ttv[c('xu','xm')], ttv[c('yu','ym')], ttv[c('zu','zm')], color='lightgrey')
segments3d(ttv[c('xu','xl')], ttv[c('yu','yl')], ttv[c('zu','zl')], color='lightgrey')
segments3d(ttv[c('xs','xm')], ttv[c('ys','ym')], ttv[c('zs','zm')], color='lightgrey')
segments3d(ttv[c('xs','xl')], ttv[c('ys','yl')], ttv[c('zs','zl')], color='lightgrey')
segments3d(ttv[c('xl','xm')], ttv[c('yl','ym')], ttv[c('zl','zm')], color='lightgrey')

spheres3d(0,0,0,col='grey', radius=0.01, lit=F)

spheres3d(tcsdata[,c('x','y','z')], radius=size, color=col, lit=F)

if(floor){
  vertices <- c( 
      -0.7, -0.5, -0.3, 1.0,
       0.7, -0.5, -0.3, 1.0,
       0.7,  1, -0.3, 1.0,
      -0.7,  1, -0.3, 1.0
  				)
  indices <- c( 1, 2, 3, 4 )
  
 wire3d( qmesh3d(vertices,indices), lit=F )
	}
	
if(hspin)
   play3d(spin3d(axis=c(0,0,1), rpm=20), duration=3)

if(vspin)
   play3d(spin3d(axis=c(1,0,0), rpm=20), duration=3)

}
