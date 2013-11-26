# New 3d tetracolorspace plot

# Example
# require(pavo)
# data(teal)
# plot3d(tcs(vismodel(teal)), theta=60, phi=25)

plot.vismodel <- function(vismodel, ...) {

cones <- attr(vismodel, "conenumb")

# get arguments
arg <- list(...)
col <- arg['col']
arg['col'] <- NULL

# CIE
if (cones==3) {
}

# Tetracolorspace
if (cones==4) {
    
    colorspacedat <- tcs(vismodel)

    xyz <- colorspacedat[c('x','y','z')]

    par.old <- par()$mar
    par(mar=c(0,0,0,0))

    ttv <- pavo::ttvertex  # already in pavo!
    verts <- cbind(unlist(ttv[c('xu','xs','xm','xl')]),
                   unlist(ttv[c('yu','ys','ym','yl')]),
                   unlist(ttv[c('zu','zs','zm','zl')]))
    dimnames(verts) <- list(c('u','s','m','l'), c('x','y','z'))
    
    # get all combinations of vertex coords to make facets
    sides <- verts[combn(1:4, 2), ]  
    
    # data range
    if (is.null(arg$xlim)) arg$xlim <- range(ttv[grep('x', names(ttv))])
    if (is.null(arg$ylim)) arg$ylim <- range(ttv[grep('y', names(ttv))])
    if (is.null(arg$zlim)) arg$zlim <- range(ttv[grep('z', names(ttv))])
    if (is.null(arg$r)) arg$r <- 12
    if (is.null(arg$box)) arg$box <- FALSE
    if (is.null(arg$theta)) arg$theta <- 30
    if (is.null(arg$phi)) arg$phi <- 45

    # draw blank 3d plot
    M <- do.call(persp, c(list(x=c(0,.1), y=c(0,.1), z=matrix(rep(0,4), nrow=2), 
                               border=FALSE), arg))

    # M <- persp(x=c(0,.1), y=c(0,.1), z=matrix(rep(0,4), nrow=2), box=F, r=12, border=FALSE, expand=expand, theta=theta, phi=phi, xlim=xlims, ylim=ylims, zlim=zlims)

    # add tetrahedron
    xy <- trans3d(sides[,'x'], sides[,'y'], sides[,'z'], M)
    # do.call(lines, c(xy, arg))  # facets
    lines(xy)

    # add data points
    xy <- trans3d(xyz$x, xyz$y, xyz$z, M)
    arg['col'] <- col
    do.call(points, c(xy, arg))
    
    # prettify
    # add vertex points
    xy <- trans3d(verts[,'x'], verts[,'y'], verts[,'z'], M)
    # arg$pch <- 16
    # arg$col <- c('violet','blue','green', 'red')
    # do.call(points, c(xy, arg))
    points(xy, pch=16, col=c('violet','blue','green', 'red'))

    # add achromatic center
    # points(trans3d(0, 0, 0, M), col='grey', pch=16, cex=2)
    # add achromatic line
    # lines(trans3d(c(0,0), c(0,0), c(-.25,.75), M), col='grey', pch=16, cex=2, lty=2)

    # TODO: add convex hulls

    par(mar=par.old)
}

}


points.vismodel <- function(vismodel, ...) {

}


plot(vismodel(teal), r=10, theta=15, phi=10, col=spec2rgb(teal), pch=16, cex=1, type='p', lwd=1)

