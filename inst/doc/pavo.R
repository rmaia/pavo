## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ---- echo=FALSE, eval=TRUE, results='hide', include=FALSE---------------
specs <- getspec(system.file("extdata", package = "pavo"), ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)

## ---- echo=TRUE, eval=FALSE, results='hide', include=TRUE----------------
#  #specs <- getspec("~/github/pavo/vignette_data/", ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
#  specs <- getspec(system.file("extdata", package = "pavo"), ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
#  # 213  files found; importing spectra
#  # ============================================================

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
specs[1:10,1:4]
dim(specs) # the data set has 213 spectra, from 300 to 700 nm, plus a 'wl' column

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
is.rspec(specs)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# Create some fake reflectance data with wavelength column arbitrarily titled 
# and not first in the data frame:
fakedat <- data.frame(refl1 = rnorm(n = 801), 
                      refl2 = rnorm(n = 801), 
                      wavelength = seq(300, 700, by = .5))
head(fakedat)

is.rspec(fakedat)

fakedat.new <- as.rspec(fakedat)

is.rspec(fakedat.new)

head(fakedat.new)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
head(as.rspec(fakedat, whichwl = 3))

## ---- echo=TRUE, eval=TRUE, fig.align='center', fig.height=4, fig.width=5----
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 500))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = 'l', xlab = 'wl')

## ---- echo=TRUE, eval=TRUE, fig.align='center', fig.height=4, fig.width=5----
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 1000))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = 'l')

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
specs.tanager1 <- subset(specs, "tanager")

head(specs.tanager1)[1:5]

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# extract first component of filenames containing species names
spp <- do.call(rbind, strsplit(names(specs), "\\."))[, 1]

# subset
specs.tanager2 <- subset(specs, subset = spp == "tanager")

# compare subsetting methods
all.equal(specs.tanager1, specs.tanager2)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
specs.tanager <- subset(specs, "tanager")
specs.parakeet <- subset(specs, "parakeet")
specs.new <- merge(specs.tanager, specs.parakeet)

## ---- label=explorespecfig, fig=TRUE, include=TRUE, fig.align='center', fig.width=7, fig.height=5.5, fig.cap="_Result from `explorespec`, showing the three measurements for each individual cardinal in separate panels_"----
# 36 spectra plus the first (wl) column
explorespec(specs[,1:37], by=3, lwd=2) 

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
mspecs <- aggspec(specs, by = 3, FUN = mean)
mspecs[1:5, 1:4]
dim(mspecs) # data now has 71 spectra, one for each individual, and the 'wl' column

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# create a vector with species identity names
spp <- gsub('\\.[0-9].*$', '', names(mspecs))[-1]
table(spp)

## ---- label=exploresppmeans, fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.cap="_Result from `explorespec` for species means_"----
sppspec <- aggspec(mspecs, by = spp, FUN = mean)
round(sppspec[1:5, ],2)
explorespec(sppspec, by = 6, lwd = 3)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=5, fig.align='center'----
plotsmooth(sppspec, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=5, fig.align='center', fig.cap="_Result for raw (grey line) and smoothed (red line) reflectance data for the parakeet_"----
spec.sm <- procspec(sppspec, opt='smooth', span = 0.2)
plot(sppspec[, 5] ~ sppspec[, 1], type = 'l', lwd = 10, col = 'grey', 
     xlab = "Wavelength (nm)", ylab = "Reflectance (%)")
lines(spec.sm[, 5] ~ sppspec[, 1], col = 'red', lwd = 2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Run some different normalizations
specs.max <- procspec(sppspec, opt='max')
specs.min <- procspec(sppspec, opt='min')
specs.str <- procspec(sppspec, opt=c('min', 'max'))  # multiple options

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=3, fig.align='center', fig.cap="_Results for max (left), min (center), and both normalizations (right)_"----
# Plot results
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2), oma = c(3, 3, 0, 0))

plot(specs.min[, 5] ~ c(300:700), xlab = "", ylab = "", type = 'l')
abline(h = 0, lty = 2)

plot(specs.max[, 5] ~ c(300:700), ylim = c(0, 1), xlab = "", ylab = "", type = 'l')
abline(h = c(0, 1), lty = 2)

plot(specs.str[, 5] ~ c(300:700), type = 'l', xlab = "", ylab = "")
abline(h = c(0, 1), lty = 2)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# pca analysis
spec.bin <- procspec(sppspec, opt = c('bin', 'center'))
head(spec.bin)
spec.bin <- t(spec.bin)  # transpose so wavelength are variables for the PCA
colnames(spec.bin) <- spec.bin[1, ]  # names variables as wavelength bins
spec.bin <- spec.bin[-1, ]  # remove 'wl' column
pca1 <- prcomp(spec.bin, scale = TRUE)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
summary(pca1)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=3, fig.align='center', fig.cap="_Plot of PC1 loading versus wavelength (left) and species mean spectra sorted vertically from lowest to highest PC1 value (right; values on right hand axis are column identities)._"----

# Generate colors from spectra
colr <- spec2rgb(sppspec)
wls <- as.numeric(colnames(spec.bin))

# Rank specs by PC1
sel <- rank(pca1$x[, 1])
sel <- match(names(sort(sel)), names(sppspec))

# Plot results
par(mfrow = c(1,2), mar = c(2, 4, 2, 2), oma = c(2, 0, 0, 0))
plot(pca1$r[,1] ~ wls, type = 'l', ylab = "PC1 loading")
abline(h = 0, lty = 2)
plot(sppspec, select = sel, type = 's', col = spec2rgb(sppspec))
mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Create a duplicate spectrum and add some negative values
refl <- sppspec[, 7] - 20
testspecs <- as.rspec(cbind(c(300:700), refl))

# Apply two different processing options
testspecs.fix1 <- procspec(testspecs, fixneg = 'addmin')
testspecs.fix2 <- procspec(testspecs, fixneg = 'zero')

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4, fig.align='center', fig.cap="_Plots showing original reflectance curve including negative values (left) and two processed curves using `fixneg = addmin` (top right) and `fixneg = zero` (bottom right)._"----
# Plot it
par(mar = c(2, 2, 2, 2), oma = c(3, 3, 0, 0))
layout(cbind(c(1, 1), c(2, 3)), widths = c(2, 1, 1))

plot(testspecs, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

plot(testspecs.fix1, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

plot(testspecs.fix2, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=5, fig.align='center', fig.cap="_Overlay plot of the teal angle-dependent reflectance with colors of each curve being an approximation of the perceived color._"----
par(mar = c(4, 4, 2, 2))
data(teal)
plot(teal, type='o', col = spec2rgb(teal))

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=7, fig.align='center', fig.cap="_Stack plot of the raw (left) and normalized (right) teal angle-dependent reflectance_"----
teal.norm <- procspec(teal, opt=c('min', 'max'))
par(mfrow=c(1,2), mar=c(2,2,2,2), oma=c(2,2,0,0))

plot(teal, type='s', col=spec2rgb(teal))
plot(teal.norm, type='s', col=spec2rgb(teal))

mtext("Wavelength (nm)", side=1, outer=T, line=1)
mtext("Cumulative reflectance (A.U.)", side=2, outer=T, line=1)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
angles <- seq(15, 70, by = 5)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=5, fig.align='center', fig.cap="_Heatmap plot for angle-resolved reflectance measurements of the green-winged teal._"----
teal.sm <- procspec(teal, opt = c('smooth'))

plot(teal.sm, type = 'h', varying = angles, 
     ylab = expression(paste("Incident angle (", degree, ")")), 
     las = 1, useRaster = TRUE)

## ---- fig=TRUE, include=TRUE, fig.width=8, fig.height=4, fig.align='center', fig.cap="_Example plots created using `aggplot`. Left: using median, standard deviation, and colored lines. Right: using mean, standard error, and greyscale_"----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2), oma = c(2, 0, 0, 0))

# Plot using median and standard deviation, default colors
aggplot(mspecs, spp, FUN.center = median, alpha = 0.3)

# Plot using mean and standard error, in greyscale
aggplot(mspecs, spp, FUN.error = function(x)sd(x)/sqrt(length(x)), 
        lcol = 1, shadecol = 'grey',  alpha = 0.7)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
summary(spec.sm)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm),2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
summary(spec.sm, subset = TRUE)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm, subset = TRUE),2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Extract only brightness variables
summary(spec.sm, subset = c('B1', 'B2', 'B3'))

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm, subset = c('B1', 'B2', 'B3')), 2)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=7, fig.align='center', fig.cap="_Plots from `peakshape`_"----
par(mfrow = c(2, 3), mar = c(5, 4, 0.5, 0.5) + 0.1)
peakshape(spec.sm, plot = TRUE)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=6, fig.align='center', fig.cap="_Plot from `peakshape`, setting the wavelength limits to 300 and 500 nm_"----
peakshape(spec.sm, select = 2, lim = c(300, 500), plot = TRUE)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
segclass(spec.sm)

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=7, fig.height=5, fig.align='center', fig.cap="_Idealized reflectance spectra and their projection on the axes of segment classification_"----
# creating idealized specs with varying hue
fakedata1 <-  sapply(seq(100,500,by = 20), 
                     function(x) rowSums(cbind(dnorm(300:700,x,30), 
                                               dnorm(300:700,x+400,30))))

# creating idealized specs with varying saturation
fakedata2 <- sapply(c(500, 300, 150, 105, 75, 55, 40, 30), 
                     function(x) dnorm(300:700,550,x))

# combining and converting to rspec
fakedata.c <- data.frame(wl = 300:700, fakedata1, fakedata2)
fakedata.c <- as.rspec(fakedata.c)
fakedata.c <- procspec(fakedata.c, "max")

fakedata1 <- as.rspec(data.frame(wl = 300:700, fakedata1))
fakedata1 <- procspec(fakedata1, "max")
fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
fakedata2 <- procspec(fakedata2, "max")

# segment classification analysis
seg.fdc <- segclass(fakedata.c)

# plot results
layout(cbind(1, 2, 3), widths = c(1, 1, 3))

par(mar = c(5, 4, 2, 0.5))
plot(fakedata1, type = 'stack', col = spec2rgb(fakedata1)) 

par(mar = c(5, 2.5, 2, 1.5))
plot(fakedata2, type = 'stack', col = spec2rgb(fakedata2)) 

par(mar = c(5, 4, 2, 0.5))
plot(seg.fdc, pch = 20, cex = 3, col = spec2rgb(fakedata.c))

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
vismod1 <- vismodel(sppspec, visual = "avg.uv", illum = 'D65', relative = FALSE)
vismod1

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(vismod1, 4)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
summary(vismod1)

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=7, fig.height=6, fig.align='center', fig.cap="_Plots of species mean reflectance curves with corresponding relative usml cone stimulations (insets)._"----
par(mfrow = c(2, 6), oma = c(3, 3, 0, 0))
layout(rbind(c(2, 1, 4, 3, 6, 5), c(1, 1, 3, 3, 5, 5), c(8, 7, 10, 9, 12, 11), c(7, 7, 9, 9, 11, 11)))

for (i in 1:6) {
  par(mar=c(2,2,2,2))
  plot(sppspec, select = i + 1, col = spec2rgb(sppspec)[i], lwd = 3, ylim = c(0,100))
  par(mar=c(4.1, 2.5, 2.5, 2))
  barplot(as.matrix(vismod1[i, 1:4]), yaxt = 'n', col = 'black')
}

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Idealized dichromat photoreceptors created using `sensmodel`._"----
idealizeddichromat <- sensmodel(c(350, 650))
plot(idealizeddichromat, col = spec2rgb(idealizeddichromat), ylab = 'Absorbance')

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
vismod.idi <- vismodel(sppspec, visual = idealizeddichromat, relative = FALSE)
vismod.idi

## ---- echo=FALSE, eval=TRUE----------------------------------------------
sapply(vismod.idi, function(x) round(x,4))

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
coldist(vismod1, vis = 'tetra', noise = 'neural', n1 = 1, n2 = 2, n3 = 2, n4 = 4, weber = 0.1)
coldist(vismod.idi, vis = 'di', n1 = 1, n2 = 1,  weber = 0.05)

## ---- echo = TRUE, eval = TRUE, results = 'hide'-------------------------
coldist(vismod1, subset = 'cardinal')

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
coldist(vismod1, subset = c('cardinal', 'jacana'))

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
vismod2 <- vismodel(sppspec)
tcs(vismod2)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(tcs(vismod2), 2)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
dist(vismod2[, 1:4])
dist(tcs(vismod2)[, c('u', 's', 'm', 'l')])

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
summary(tcs(vismod2))

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
tcs.mspecs <- tcs(vismodel(mspecs))
summary(tcs.mspecs, by = spp)

## ---- echo=TRUE, eval=FALSE, results='hide'------------------------------
#  tcsplot(tcs.mspecs, col = spec2rgb(mspecs), size = 0.01)
#  # rgl.postscript('pavo-tcsplot.pdf',fmt='pdf')

## ---- echo=TRUE, eval=FALSE, results='hide'------------------------------
#  tcsplot(tcs(vismod2), size=0)
#  tcsvol(tcs(vismod2))
#  # rgl.snapshot('pavo-tcsvolplot.png')

## ---- echo=FALSE, out.width = 500, fig.retina = NULL, fig.align='center', fig.cap="_Example plots obtained using `tcsplot`. Plot on the left was exported as pdf, while the one on the right was exported as png (`tcsplot` uses the `rgl` package for interactive 3D plotting capabilities, and `rgl` does not currently support transparency when exporting as `pdf`)._"----
knitr::include_graphics("fig/pavo-tcsplot-combined.png")

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Projection plot from a tetrahedral color space._"----
projplot(tcs.mspecs, pch = 20, col = spec2rgb(mspecs))

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_`aggplot` of the `sicalis` data (blue: crown, red: throat, green: breast)_"----
data(sicalis)
aggplot(sicalis, by=rep(c('C','T','B'), 7))

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
tcs.sicalis.C <- subset(tcs(vismodel(sicalis)), 'C')
tcs.sicalis.T <- subset(tcs(vismodel(sicalis)), 'T')
tcs.sicalis.B <- subset(tcs(vismodel(sicalis)), 'B')
#voloverlap(tcs.sicalis.T,tcs.sicalis.B, plot=T)
#voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T) 
voloverlap(tcs.sicalis.T, tcs.sicalis.B)
voloverlap(tcs.sicalis.T, tcs.sicalis.C)

## ---- echo=FALSE, out.width = 500, fig.retina = NULL, fig.align='center', fig.cap="_Color volume overlaps. Shaded area in panel a represents the overlap between those two sets of points._"----
knitr::include_graphics("fig/pavo-overlap-combined.png")

