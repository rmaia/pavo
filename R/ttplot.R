#' Plot a Tetrahedral Color Space
#'
#' Produces a 3D plot of a tetrahedral color space using OpenGL capabilities
#'
#' @import rgl
#' @param tcres a data frame, possibly a result from the \code{tcs} function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param size size of the points in the plot
#' @param col color of the points in the plot
#' @param new should a new 3D plot be called?
#' @param hspin if \code{TRUE}, the grapihic will spin horizontally (around the 'z' axis)
#' @param vspin if \code{TRUE}, the grapihic will spin vertically (around the 'x' axis)
#' @param floor if \code{TRUE}, a reference xy plane is plotted under the tetrahedron
#' @param grid if \code{TRUE}, connects the polygon outlining the volume occupied by points
#' @param fill if \code{TRUE}, fills the volume occupied by points (WARNING: transparency
#' is not saved properly if exported using \code{rgl.postscript})
#' @return \code{ttplot} creates a 3D plot using functions of the package \code{rgl}, 
#' based on openGL capabilities. Plot is interactive and can be manipulated with the mouse 
#' (left button: rotate along 'z' axis; right button: rotate along 'x' axis; 
#' third button: zoom). \code{ttvol} creates polygon based on points, determining the volume
#' occupied by them in the colorspace. \code{ttpoints} adds points to the plot. Points are
#' currently plotted only as spheres to maintain export capabilities.
#' @export ttplot ttpoints ttvol
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis, by=rep(c('C','T','B'),7))
#' ttplot(tcs.sicalis, size=0.005)
#' ttvol(tcs.sicalis)
#' ttvol(tcs.sicalis$tcs[grep('C$',row.names(tcs.sicalis$tcs)),],col='red')
#' ttvol(tcs.sicalis$tcs[grep('B$',row.names(tcs.sicalis$tcs)),],col='violet')
#' ttvol(tcs.sicalis$tcs[grep('T$',row.names(tcs.sicalis$tcs)),],col='orange')
#' rgl.postscript('testplot.pdf',fmt='pdf') 
#' rgl.snapshot('testplot.png')}
#' @seealso \code{\link{spheres3d}},\code{\link{rgl.postscript}}, 
#' \code{\link{rgl.snapshot}},\code{\link{rgl.par3d}} 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405–431.

ttplot<- function(tcsres, size=0.02, col='black', new=T, hspin=T, vspin=F, floor=T) {

if(class(tcsres)=='tcs'){
  dat <- tcsres$tcs	
  }else{
    dat <- tcsres
    }

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

spheres3d(dat[,c('x','y','z')], radius=size, color=col, lit=F)

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


ttpoints<- function(tcsres, size=0.02, col='black'){

if(class(tcsres)=='tcs'){
  dat <- tcsres$tcs	
  }else{
    dat <- tcsres
    }


spheres3d(dat[,c('x','y','z')], radius=size, color=col, lit=F)
}

ttvol <- function(tcsres, col='black', grid=T, fill=T){

if(class(tcsres)=='tcs'){
  dat <- tcsres$tcs	
  }else{
    dat <- tcsres
    }

vol <- t(convhulln(dat[,c('x','y','z')],options='FA')$hull)
coords <- dat[,c('x','y','z')]
listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
ppairs <- do.call(rbind,lapply(listvol,function(x)t(combn(x,2))))

if(grid==T){
  for(i in 1:nrow(ppairs)){
      segments3d(coords[ppairs[i,],'x'], 
                 coords[ppairs[i,],'y'],
                 coords[ppairs[i,],'z'], color=col)
  }
}

if(fill==T)
rgl.triangles(coords[vol,1],coords[vol,2],coords[vol,3], alpha=0.2, color=col)
}