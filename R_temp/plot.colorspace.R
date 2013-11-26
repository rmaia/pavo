# New 3d tetracolorspace plot

# Example
# require(pavo)
# data(teal)
# plot3d(tcs(vismodel(teal)), theta=60, phi=25)

plot.colorspace <- function(colorspace, theta=30, phi=45,...) {

cones <- attr(colorspace, "conenumb")

# TODO: check if input is a tetracolorspace object
# stop("Not a tetracolorspace object")
if(cones==4) {

    xyz <- colorspace[c('x','y','z')]
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
    xlims <- range(ttv[grep('x', names(ttv))])
    ylims <- range(ttv[grep('y', names(ttv))])
    zlims <- range(ttv[grep('z', names(ttv))])
    # draw blank 3d plot
    M <- persp(x=c(0,.1), y=c(0,.1), z=matrix(rep(0,4), nrow=2), box=F, r=12, border=FALSE, expand=expand, theta=theta, phi=phi, xlim=xlims, ylim=ylims, zlim=zlims)
    # add tetrahedron
    lines(trans3d(sides[,'x'], sides[,'y'], sides[,'z'], M))  # facets
    # add data points
    points(trans3d(xyz$x, xyz$y, xyz$z, M), col=spec2rgb(teal), pch=16)
    # prettify
    points(trans3d(verts[,'x'], verts[,'y'], verts[,'z'], M), col=c('violet','blue','green', 'red'), pch=16)  # vertex points
    points(trans3d(0, 0, 0, M), col='grey', pch=16, cex=2)  # achromatic center
    lines(trans3d(c(0,0), c(0,0), c(-.25,.75), M), col='grey', pch=16, cex=2, lty=2)  # achromatic line

    # TODO: add convex hulls

    par(mar=par.old)

}

}
