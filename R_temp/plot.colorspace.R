# New 3d tetracolorspace plot

# Example
# require(pavo)
# data(teal)
# plot3d(tcs(vismodel(teal)), theta=60, phi=25)

require(geometry)

# TODO
# [ ] add ability to zoom in/out
# [ ] add points function
# [ ] add hull color options
# [ ] work on CIE option for conenumb=3
# [ ] MAYBE: add ability to show frame of reference when zoomed in on data (maybe tetrahedron in upper right?)

# zoom option - should it always center on the centroid of the data points? or of the 3D tetrahedron?

plot.vismodel <- function(vismodel, achro.pt=FALSE, achro.line=FALSE, hull=FALSE, 
    zoom = 1, view = c('data', 'tetrahedron'), ...) {

    cones <- attr(vismodel, "conenumb")

    # get arguments
    arg <- list(...)
    col <- arg['col']
    arg['col'] <- NULL

    if (cones==3) {

    }

    # Tetracolorspace
    if (cones==4) {
        colorspacedat <- tcs(vismodel)
        xyz <- colorspacedat[c('x','y','z')]
        # save original par settings
        par.old <- par()$mar
        par(mar=c(0,0,0,0))
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
            if (is.null(arg$xlim)) arg$xlim <- range(ttv[grep('x', names(ttv))]) / zoom
            if (is.null(arg$ylim)) arg$ylim <- range(ttv[grep('y', names(ttv))]) / zoom
            if (is.null(arg$zlim)) arg$zlim <- range(ttv[grep('z', names(ttv))]) / zoom
        }
        # zoom to show data
        if (view=="data") {
            if (is.null(arg$xlim)) arg$xlim <- range(xyz$x)
            if (is.null(arg$ylim)) arg$ylim <- range(xyz$y)
            if (is.null(arg$zlim)) arg$zlim <- range(xyz$z)
        }
        if (is.null(arg$r)) arg$r <- 12
        if (is.null(arg$box)) arg$box <- FALSE
        if (is.null(arg$theta)) arg$theta <- 30
        if (is.null(arg$phi)) arg$phi <- 45

        # draw blank 3d plot
        M <- do.call(persp, c(list(x=arg$xlim,
                                   y=arg$ylim,
                                   z=matrix(c(arg$zlim,arg$zlim), nrow=2),
                                   border=FALSE), arg))
        
        # add tetrahedron
        xy <- trans3d(sides[,'x'], sides[,'y'], sides[,'z'], M)
        # do.call(lines, c(xy, arg))  # facets
        lines(xy)
        # add data points
        xy <- trans3d(xyz$x, xyz$y, xyz$z, M)
        arg['col'] <- col
        do.call(points, c(xy, arg))
        # add vertex points
        xy <- trans3d(verts[,'x'], verts[,'y'], verts[,'z'], M)
        # arg$pch <- 16
        # arg$col <- c('violet','blue','green', 'red')
        # do.call(points, c(xy, arg))
        points(xy, pch=16, col=c('violet','blue','green', 'red'))
        # add achromatic center
        if (achro.pt) {
        points(trans3d(0, 0, 0, M), col='grey', pch=16, cex=2)
        }
        # add achromatic line
        if (achro.line) {
        lines(trans3d(c(0,0), c(0,0), c(-.25,.75), M), col='grey', pch=16, cex=2, lty=2)
        }   
       
        # add convex hulls
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
              polygon(xy, col=rgb(r=0,g=0,b=0,alpha=0.1), border=rgb(r=0,g=0,b=0,alpha=0.3))
            }
        }
        par(mar=par.old)
    }

}

# points.vismodel <- function(vismodel, ...) {

# }

data(sicalis)

plot(vismodel(sicalis), r=500, theta=50, phi=90, col=spec2rgb(sicalis), type='p',
    lwd=1, achro.pt=T, achro.line=T, hull=T, zoom=1, box=T, cex=2, view="tetrahedron")

