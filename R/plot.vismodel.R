#' New colorspace plotter
#'
#' DESCRIPTION HERE...
#'
#' @import geometry
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#' of \code{\link{vismodel}} or independently calculated data in the form of a data frame
#' with columns representing the quantum catches of different cones/receptors.
#' @param achro.pt whether to plot the achromatic center on the chromaticity diagram
#' @param achro.line whether to add a line in 3D tetrahedral plots representing the
#' achromatic center
#' @param hull whether to add convex hulls around data points
#' @param zoom zoom factor to multiple data points by (?) 
#' @param view specifies whether the plot should contain the entire chromaticity diagram
#' (default) or only the given data points
#' @param type for visual models with <4 cones, specifies the type of chromaticity
#' diagram to produce (defaults to Chittka's hexagon diagram)
#'
#' @return a chromaticity diagram
#'
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis)}
#' plot3d(tcs(vismodel(teal)), theta=60, phi=25)
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.
#'
#' @export plot.vismodel

# TODO/NOTES
# [x] add ability to zoom in/out
# [ ] add points function
# [ ] add hull color options
# [x] work on CIE option for conenumb=3
# [ ] give ability to show frame of reference when zoomed in on data (tetrahedron in upper right?)
# zoom option - should it always center on the centroid of the data points? or of the 3D tetrahedron?

plot.vismodel <- function(vismodeldat, achro.pt = FALSE, achro.line = FALSE, hull = FALSE, 
    zoom = 1, view = c('tetrahedron', 'crop'), type = c('hexagon', 'cie', 'macleod'), ...) {

  par.old <- par()$mar  # save original par settings
                          # [ ] why not save all, not just 'mar'?

  cones <- attr(vismodeldat, "conenumb")
  view <- match.arg(view)
  type <- match.arg(type)

  # get arguments
  arg <- list(...)
  if ('col' %in% names(arg)) {
    col <- arg['col']
  } else {
    col <- 'black'
  }
  arg['col'] <- NULL  # need this so colors won't apply to tetrahedron

  if (cones == 2) {
    stop('Plotting methods not yet defined for 2-cone visual systems')
  }


  if (cones == 3) {

      if (type=="hexagon") {
          # convert ubg catches to xy coordinates (equations in Chittka 1992)
          ubg2xy <- function(u, b, g) {
            # u <- u/(u+1)  # not sure about whether we need this normalization
            # b <- b/(b+1)
            # g <- g/(g+1)
            x <- (sqrt(3) / 2) * (g - u)
            y <- b - 0.5 * (u + g)
            data.frame(x, y)
          }
          # coordinates of hexagon:
          ubgcoords <- cbind(u = c(1, 1, 0, 0, 0, 1), b = c(0, 0, 0, 1, 1, 1),
            g = c(0, 1, 1, 1, 0, 0))
          hexcoords <- ubg2xy(ubgcoords[, 1], ubgcoords[, 2], ubgcoords[, 3])
          # generate plot
          plot(hexcoords, type = 'n', asp = 1, ...)
          polygon(hexcoords)
          text(hexcoords[c(1,5,3), ], labels=c("UV", "B", "G"))
          points(ubg2xy(u = vismodeldat[, 1], b = vismodeldat[, 2], g = vismodeldat[, 3]), ...)
      }

      if (type=="cie") {
          
          # chromaticity coordinates of spectral loci (REF?)
          ciex <- c(0.175596, 0.172787, 0.170806, 0.170085, 0.160343, 0.146958, 0.139149,
                    0.133536, 0.126688, 0.115830, 0.109616, 0.099146, 0.091310, 0.078130,
                    0.068717, 0.054675, 0.040763, 0.027497, 0.016270, 0.008169, 0.004876,
                    0.003983, 0.003859, 0.004646, 0.007988, 0.013870, 0.022244, 0.027273,
                    0.032820, 0.038851, 0.045327, 0.052175, 0.059323, 0.066713, 0.074299,
                    0.089937, 0.114155, 0.138695, 0.154714, 0.192865, 0.229607, 0.265760,
                    0.301588, 0.337346, 0.373083, 0.408717, 0.444043, 0.478755, 0.512467,
                    0.544767, 0.575132, 0.602914, 0.627018, 0.648215, 0.665746, 0.680061,
                    0.691487, 0.700589, 0.707901, 0.714015, 0.719017, 0.723016, 0.734674)
          ciey <- c(0.005295, 0.004800, 0.005472, 0.005976, 0.014496, 0.026643, 0.035211,
                    0.042704, 0.053441, 0.073601, 0.086866, 0.112037, 0.132737, 0.170464,
                    0.200773, 0.254155, 0.317049, 0.387997, 0.463035, 0.538504, 0.587196,
                    0.610526, 0.654897, 0.675970, 0.715407, 0.750246, 0.779682, 0.792153,
                    0.802971, 0.812059, 0.819430, 0.825200, 0.829460, 0.832306, 0.833833,
                    0.833316, 0.826231, 0.814796, 0.805884, 0.781648, 0.754347, 0.724342,
                    0.692326, 0.658867, 0.624470, 0.589626, 0.554734, 0.520222, 0.486611,
                    0.454454, 0.424252, 0.396516, 0.372510, 0.351413, 0.334028, 0.319765,
                    0.308359, 0.299317, 0.292044, 0.285945, 0.280951, 0.276964, 0.265326)

          plot(ciey ~ ciex, type = 'n', ...)
          points(y ~ x, vismodeldat, ...)
          polygon(ciey ~ ciex)

      }

      if (type=="macleod") {
        # SOME CODE HERE
        # AND HERE
      }
  }


  if (cones == 4) {

      # CE: Are we wanting to do this some other way?
      colorspacedat <- tcs(vismodeldat)
      xyz <- colorspacedat[c('x','y','z')]
      
      par(mar = c(0, 0, 0, 0))
      
      # get tetra colorspace vertices
      ttv <- pavo::ttvertex
      verts <- cbind(unlist(ttv[c('xu','xs','xm','xl')]),
                     unlist(ttv[c('yu','ys','ym','yl')]),
                     unlist(ttv[c('zu','zs','zm','zl')]))
      dimnames(verts) <- list(c('u','s','m','l'), c('x','y','z'))
      
      # get all combinations of vertex coords to make facets
      sides <- verts[combn(1:4, 2), ]  
      
      # compute data range (show whole tetrahedron)
      if (view=="tetrahedron") {
          if (is.null(arg$xlim)) {
            arg$xlim <- range(ttv[grep('x', names(ttv))]) / zoom
          }
          if (is.null(arg$ylim)) {
            arg$ylim <- range(ttv[grep('y', names(ttv))]) / zoom
          }
          if (is.null(arg$zlim)) {
            arg$zlim <- range(ttv[grep('z', names(ttv))]) / zoom
          }
      }

      # zoom to show data
      if (view=="crop") {
          if (is.null(arg$xlim)) arg$xlim <- range(xyz$x)
          if (is.null(arg$ylim)) arg$ylim <- range(xyz$y)
          if (is.null(arg$zlim)) arg$zlim <- range(xyz$z)
      }

      # set sensible default plot parameters
      if (is.null(arg$r)) {
        arg$r <- 12
      }
      if (is.null(arg$box)) {
        arg$box <- FALSE
      }
      if (is.null(arg$theta)) {
        arg$theta <- 30
      }
      if (is.null(arg$phi)) {
        arg$phi <- 45
      }

      # draw blank 3d plot
      M <- do.call(persp, c(list(x = arg$xlim, y = arg$ylim,
                                 z = matrix(c(arg$zlim, arg$zlim), nrow = 2),
                                 border = FALSE), arg))
      
      # add tetrahedron
      xy <- trans3d(sides[,'x'], sides[,'y'], sides[,'z'], M)
      lines(xy, col = 'black')
      
      # add data points
      xy <- trans3d(xyz$x, xyz$y, xyz$z, M)
      arg['col'] <- col
      # arg['col'] <- 'pink'
      arg0 <- arg[names(arg) %in% c(names(formals(points.default)), names(par()))]
      do.call(points, c(xy, arg0))
      
      # add vertex points
      xy <- trans3d(verts[,'x'], verts[,'y'], verts[,'z'], M)
      points(xy, pch=16, col = c('violet', 'blue', 'green', 'red'))
      
      if (achro.pt) {
          points(trans3d(0, 0, 0, M), col='grey', pch=16, cex=2)
      }
      
      if (achro.line) {
          lines(trans3d(c(0,0), c(0,0), c(-.25,.75), M), col='grey', pch=16, cex=2, lty=2)
      }   
     
      if (hull) {
          # glossycoords <- subset(xyz, final3$type.x=="glossy")
          vol <- t(convhulln(xyz, options = "FA")$hull)
          listpts <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
          # pairids <- do.call(rbind, lapply(listglossy, function(x) t(combn(x, 2))))
          # glossyxyz <- data.frame(plottemplate$xyz.convert(glossycoords[,1:3]))
          for(i in 1:ncol(vol)) {
            xy <- trans3d(x=xyz[vol[,i], 1],
                    y=xyz[vol[,i], 2],
                    z=xyz[vol[,i], 3],
                    M)
            # [ ] give option to change hull colors
            polygon(xy, col = rgb(r=0,g=0,b=0,alpha=0.1), border = rgb(r=0,g=0,b=0,alpha=0.3))
          }
      }
  }

  par(mar=par.old)

}

# points.vismodel <- function(vismodel, ...) {

# }

# data(sicalis)

# plot(vismodel(sicalis), r=500, theta=50, phi=90, col=spec2rgb(sicalis), type='p',
    # lwd=1, achro.pt=T, achro.line=T, hull=T, zoom=1, box=T, cex=2, view="tetrahedron")

