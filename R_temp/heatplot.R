# Make heat plot
heatplot <- function(specs, varying, col=heat.colors(25)) {
wl <- specs[1, ]
specs <- specs[-1, ]
dat <- sapply(1:ncol(specs), function(z){approx(x=varying, y=specs[,z], n=100)$y})
image(x=wl, y=approx(varying, n=100)$y, z=t(dat), col=col)
}
