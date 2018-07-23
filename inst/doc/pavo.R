## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ---- echo = FALSE, fig.cap = "_A non-exhaustive overview of the colour-pattern analysis workflow in pavo, as of version 2.0, displaying some key functions at each stage._", out.width = '70%', dpi = 72----
knitr::include_graphics('fig/workflow.png')

## ---- echo=FALSE, eval=TRUE, results='hide', include=FALSE---------------
# specs <- getspec(system.file("extdata", package = "pavo"), ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
specs <- readRDS(system.file("extdata/specsdata.rda", package = "pavo"))

## ---- echo=TRUE, eval=FALSE, results='hide', include=TRUE----------------
#  #specs <- getspec("~/pavo/vignette_data/", ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
#  # 213  files found; importing spectra
#  # |================================================================================| 100%, ETA 00:00

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
specs[1:10, 1:4]
dim(specs) # the data set has 213 spectra, from 300 to 700 nm, plus a 'wl' column

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
is.rspec(specs)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# Create some fake reflectance data with wavelength column arbitrarily titled
# and not first in the data frame:
fakedat <- data.frame(
  refl1 = rnorm(n = 801),
  refl2 = rnorm(n = 801),
  wavelength = seq(300, 700, by = .5)
)
head(fakedat)

is.rspec(fakedat)

fakedat.new <- as.rspec(fakedat)

is.rspec(fakedat.new)

head(fakedat.new)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
head(as.rspec(fakedat, whichwl = 3))

## ---- echo=TRUE, eval=TRUE, fig.align='center', fig.height=3, fig.width=4----
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 500))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = "l", xlab = "wl")

## ---- echo=TRUE, eval=TRUE, fig.align='center', fig.height=3, fig.width=4----
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 1000))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = "l")

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

## ---- label=explorespecfig, fig=TRUE, include=TRUE, fig.align='center', fig.width=6, fig.height=4.5, fig.cap="_Result from `explorespec`, showing the three measurements for each individual cardinal in separate panels_"----
# 36 spectra plus the first (wl) column
explorespec(specs[, 1:37], by = 3, lwd = 2) 

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
round(sppspec[1:5, ], 2)
explorespec(sppspec, by = 6, lwd = 3)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4.5, fig.align='center'----
plotsmooth(sppspec, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Result for raw (grey line) and smoothed (red line) reflectance data for the parakeet_"----
spec.sm <- procspec(sppspec, opt = "smooth", span = 0.2)
plot(sppspec[, 5] ~ sppspec[, 1],
  type = "l", lwd = 10, col = "grey",
  xlab = "Wavelength (nm)", ylab = "Reflectance (%)"
)
lines(spec.sm[, 5] ~ sppspec[, 1], col = "red", lwd = 2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Run some different normalizations
specs.max <- procspec(sppspec, opt = "max")
specs.min <- procspec(sppspec, opt = "min")
specs.str <- procspec(sppspec, opt = c("min", "max")) # multiple options

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=3, fig.align='center', fig.cap="_Results for min (left), max (center), and both normalizations (right)_"----
# Plot results
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2), oma = c(3, 3, 0, 0))

plot(specs.min[, 5] ~ c(300:700), xlab = "", ylab = "", type = "l")
abline(h = 0, lty = 2)

plot(specs.max[, 5] ~ c(300:700), ylim = c(0, 1), xlab = "", ylab = "", type = "l")
abline(h = c(0, 1), lty = 2)

plot(specs.str[, 5] ~ c(300:700), type = "l", xlab = "", ylab = "")
abline(h = c(0, 1), lty = 2)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# pca analysis
spec.bin <- procspec(sppspec, opt = c("bin", "center"))
head(spec.bin)
spec.bin <- t(spec.bin) # transpose so wavelength are variables for the PCA
colnames(spec.bin) <- spec.bin[1, ] # names variables as wavelength bins
spec.bin <- spec.bin[-1, ] # remove 'wl' column
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
par(mfrow = c(1, 2), mar = c(2, 4, 2, 2), oma = c(2, 0, 0, 0))
plot(pca1$r[, 1] ~ wls, type = "l", ylab = "PC1 loading")
abline(h = 0, lty = 2)
plot(sppspec, select = sel, type = "s", col = spec2rgb(sppspec))
mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Create a duplicate spectrum and add some negative values
refl <- sppspec[, 7] - 20
testspecs <- as.rspec(cbind(c(300:700), refl))

# Apply two different processing options
testspecs.fix1 <- procspec(testspecs, fixneg = "addmin")
testspecs.fix2 <- procspec(testspecs, fixneg = "zero")

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.align='center', fig.cap="_Plots showing original reflectance curve including negative values (left) and two processed curves using `fixneg = addmin` (top right) and `fixneg = zero` (bottom right)._"----
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

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Overlay plot of the teal angle-dependent reflectance with colors of each curve being an approximation of the perceived color._"----
par(mar = c(4, 4, 2, 2))
data(teal)
plot(teal, type = "o", col = spec2rgb(teal))

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=5.5, fig.align='center', fig.cap="_Stack plot of the raw (left) and normalized (right) teal angle-dependent reflectance_"----
teal.norm <- procspec(teal, opt = c("min", "max"))
par(mfrow = c(1, 2), mar = c(2, 2, 2, 2), oma = c(2, 2, 0, 0))

plot(teal, type = "s", col = spec2rgb(teal))
plot(teal.norm, type = "s", col = spec2rgb(teal))

mtext("Wavelength (nm)", side = 1, outer = T, line = 1)
mtext("Cumulative reflectance (A.U.)", side = 2, outer = T, line = 1)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
angles <- seq(15, 70, by = 5)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Heatmap plot for angle-resolved reflectance measurements of the green-winged teal._"----
teal.sm <- procspec(teal, opt = c("smooth"))

plot(teal.sm,
  type = "h", varying = angles,
  ylab = expression(paste("Incident angle (", degree, ")")),
  las = 1, useRaster = TRUE
)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4, fig.align='center', fig.cap="_Example plots created using `aggplot`. Left: using median, standard deviation, and colored lines. Right: using mean, standard error, and greyscale_"----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2), oma = c(2, 0, 0, 0))

# Plot using median and standard deviation, default colors
aggplot(mspecs, spp, FUN.center = median, alpha = 0.3, legend = TRUE)

# Plot using mean and standard error, in greyscale
aggplot(mspecs, spp,
  FUN.error = function(x) sd(x) / sqrt(length(x)),
  lcol = 1, shadecol = "grey", alpha = 0.7
)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
summary(spec.sm)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm), 2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
summary(spec.sm, subset = TRUE)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm, subset = TRUE), 2)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
# Extract only brightness variables
summary(spec.sm, subset = c('B1', 'B2', 'B3'))

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(summary(spec.sm, subset = c('B1', 'B2', 'B3')), 2)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Plots from `peakshape`_"----
par(mfrow = c(2, 3))
peakshape(spec.sm, plot = TRUE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Plot from `peakshape`, setting the wavelength limits to 300 and 500 nm_"----
peakshape(spec.sm, select = 2, lim = c(300, 500), plot = TRUE)

## ----echo=TRUE, eval=TRUE------------------------------------------------
musca_sense <- sensdata(visual = "musca", achromatic = "md.r1")
head(musca_sense)

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
vismod1 <- vismodel(sppspec,
  visual = "avg.uv", achromatic = "bt.dc",
  illum = "D65", relative = FALSE
)
vismod1

## ---- echo=FALSE, eval=TRUE----------------------------------------------
round(vismod1, 4)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
summary(vismod1)

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=6, fig.height=5, fig.align='center', fig.cap="_Plots of species mean reflectance curves with corresponding relative usml cone stimulations (insets)._"----
par(mfrow = c(2, 6), oma = c(3, 3, 0, 0))
layout(rbind(c(2, 1, 4, 3, 6, 5), c(1, 1, 3, 3, 5, 5), c(8, 7, 10, 9, 12, 11), c(7, 7, 9, 9, 11, 11)))

sppspecol <- as.character(spec2rgb(sppspec))

for (i in 1:6) {
  par(mar = c(2, 2, 2, 2))
  plot(sppspec, select = i + 1, col = sppspecol[i], lwd = 3, ylim = c(0, 100))
  par(mar = c(4.1, 2.5, 2.5, 2))
  barplot(as.matrix(vismod1[i, 1:4]), yaxt = "n", col = "black")
}

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Idealized dichromat photoreceptors created using `sensmodel`._"----
idealizeddichromat <- sensmodel(c(350, 650))
plot(idealizeddichromat, col = spec2rgb(idealizeddichromat), ylab = "Absorbance")

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
vismod.idi <- vismodel(sppspec, visual = idealizeddichromat, relative = FALSE)
vismod.idi

## ---- echo=FALSE, eval=TRUE----------------------------------------------
sapply(vismod.idi, function(x) round(x, 4))

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
coldist(vismod1,
  noise = "neural", achro = TRUE, n = c(1, 2, 2, 4),
  weber = 0.1, weber.achro = 0.1
)
coldist(vismod.idi, n = c(1, 2), weber = 0.1)

## ---- echo = TRUE, eval = TRUE, results = 'hide'-------------------------
coldist(vismod1, subset = 'cardinal')

## ---- echo=TRUE, eval=TRUE, results='hide'-------------------------------
coldist(vismod1, subset = c('cardinal', 'jacana'))

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  fakedata1 <- sapply(
#    seq(100, 500, by = 20),
#    function(x) rowSums(cbind(
#        dnorm(300:700, x, 30),
#        dnorm(300:700, x + 400, 30)
#      ))
#  )
#  
#  # Creating idealized specs with varying saturation
#  fakedata2 <- sapply(
#    c(500, 300, 150, 105, 75, 55, 40, 30),
#    function(x) dnorm(300:700, 550, x)
#  )
#  
#  fakedata1 <- as.rspec(data.frame(wl = 300:700, fakedata1))
#  fakedata1 <- procspec(fakedata1, "max")
#  fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
#  fakedata2 <- procspec(fakedata2, "sum")
#  fakedata2 <- procspec(fakedata2, "min")
#  
#  # Converting reflectance to percentage
#  fakedata1[, -1] <- fakedata1[, -1] * 100
#  fakedata2[, -1] <- fakedata2[, -1] / max(fakedata2[, -1]) * 100
#  
#  # Combining and converting to rspec
#  fakedata.c <- data.frame(wl = 300:700, fakedata1[, -1], fakedata2[, -1])
#  fakedata.c <- as.rspec(fakedata.c)

## ---- echo=FALSE, include=FALSE------------------------------------------
fakedata1 <- sapply(
  seq(100, 500, by = 20),
  function(x) rowSums(cbind(
      dnorm(300:700, x, 30),
      dnorm(300:700, x + 400, 30)
    ))
)

# Creating idealized specs with varying saturation
fakedata2 <- sapply(
  c(500, 300, 150, 105, 75, 55, 40, 30),
  function(x) dnorm(300:700, 550, x)
)

fakedata1 <- as.rspec(data.frame(wl = 300:700, fakedata1))
fakedata1 <- procspec(fakedata1, "max")
fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
fakedata2 <- procspec(fakedata2, "sum")
fakedata2 <- procspec(fakedata2, "min")

# Converting reflectance to percentage
fakedata1[, -1] <- fakedata1[, -1] * 100
fakedata2[, -1] <- fakedata2[, -1] / max(fakedata2[, -1]) * 100

# Combining and converting to rspec
fakedata.c <- data.frame(wl = 300:700, fakedata1[, -1], fakedata2[, -1])
fakedata.c <- as.rspec(fakedata.c)

## ------------------------------------------------------------------------
# Visual model and color distances
fakedata.vm <- vismodel(fakedata.c, relative = FALSE, achro = TRUE)
fakedata.cd <- coldist(fakedata.vm,
  noise = "neural", n = c(1, 2, 2, 4),
  weber = 0.1, achro = TRUE
)

# Converting to Cartesian coordinates
fakedata.cc <- jnd2xyz(fakedata.cd, ref1 = "l", axis1 = c(1, 0, 0), ref2 = NULL)
head(fakedata.cc)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.align='center', fig.cap="_Spectral data in a receptor noise-corrected colorspace_"----
plot(fakedata.cc, theta = 55, phi = 25, col = spec2rgb(fakedata.c))

## ----fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_The flower dataset_"----
data(flowers)

flowercols <- spec2rgb(flowers)

head(flowers[1:4])
plot(flowers, lwd = 2, col = flowercols)

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.align='center', fig.cap="_Flowers in a dichromatic colorspace, as modelled according to a canid visual system._"----
plot(di.flowers, col = flowercols) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'fi', scale = 10000)

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.align='center', fig.cap="_Floral reflectance in a Maxwell triangle, considering a honeybee visual system._"----
plot(tri.flowers, pch = 21, bg = flowercols) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "bluetit", qcatch = "fi", scale = 10000)

tetra.flowers <- colspace(vis.flowers, space = "tcs")

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.height=5, fig.width=5, fig.align='center', fig.cap="_Flowers in a tetrahedral colorspace modelled using the visual phenotype of the blue tit. Point size is used to force perspective_"----
plot(tetra.flowers, pch = 21, bg = flowercols, perspective = TRUE, range = c(1, 2), cex = 0.5)

## ---- fig=TRUE, include=TRUE, fig.height=4, fig.width=6, fig.align='center', fig.cap="_Flowers in a tetrahedral colorspace, with varied orientations and perspectives, modelled using the visual phenotype of the blue tit._"----
par(mfrow = c(1, 2), pty = "s")
plot(tetra.flowers, pch = 21, bg = flowercols)
axistetra(x = 0, y = 1.8)
plot(tetra.flowers, theta = 110, phi = 10, pch = 21, bg = flowercols)
axistetra(x = 0, y = 1.8)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_Projection plot from a tetrahedral color space._"----
projplot(tetra.flowers, pch = 20, col = spec2rgb(flowers))

## ------------------------------------------------------------------------
data(sicalis)

## ---- echo=TRUE, eval=TRUE, fig.show='hide'------------------------------
par(mfrow = c(1, 2), pty = "s")
tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")
voloverlap(tcs.sicalis.T, tcs.sicalis.B, plot = TRUE)
voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE)

## ---- echo=FALSE, eval=TRUE, results='hide', fig.width=6, fig.align='center', fig.cap="_Volume overlap between male Stripe-Tailed Yellow Finch (_Sicalis citrina_) throat and breast (left) and throat and crown (right)._"----
par(mfrow = c(1, 2), pty = "s")
voloverlap(tcs.sicalis.T, tcs.sicalis.B, plot = TRUE)
voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
summary(tetra.flowers)

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')

## ------------------------------------------------------------------------
hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Flowers as modelled in the hymenopteran color hexagon of Chittka (1992), overlain with coarse bee-hue sectors._"----
plot(hex.flowers, sectors = 'coarse', pch = 21, bg = flowercols)

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE, bkg = "green")

coc.flowers <- colspace(vis.flowers, space = "coc")

head(coc.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Flowers in the color-opponent-coding space of Backhaus (1991), as modelling according to the honeybee._"----
plot(coc.flowers, pch = 21, bg = flowercols, yaxt = "n")

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE, achromatic = 'none')

## ------------------------------------------------------------------------
ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
head(ciexyz.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Floral reflectance in the CIEXYZ human visual model. Note that this space is not perceptually calibrated, so we cannot make inferences about the similarity or differences of colors based on their relative location._"----
plot(ciexyz.flowers, pch = 21, bg = flowercols) 

## ------------------------------------------------------------------------
cielab.flowers <- colspace(vis.flowers, space = 'cielab')
head(cielab.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_CIELAB._"----
plot(cielab.flowers, pch = 21, bg = flowercols) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achro = 'none', relative = TRUE)

## ------------------------------------------------------------------------
cat.flowers <- colspace(vis.flowers, space = 'categorical')

head(cat.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Flowers in the categorical colorspace of Troje (1993)._"----
plot(cat.flowers, pch = 21, bg = flowercols) 

## ---- fig=TRUE, eval=TRUE, include=TRUE, results = 'hide', fig.width=4, fig.height=4, fig.align='center', fig.cap="_Idealized reflectance spectra and their projection on the axes of segment classification_"----
fakedata1 <- sapply(
  seq(100, 500, by = 20),
  function(x) rowSums(cbind(
      dnorm(300:700, x, 30),
      dnorm(300:700, x + 400, 30)
    ))
)

# creating idealized specs with varying saturation
fakedata2 <- sapply(
  c(500, 300, 150, 105, 75, 55, 40, 30),
  function(x) dnorm(300:700, 550, x)
)

fakedata1 <- as.rspec(data.frame(wl = 300:700, fakedata1))
fakedata1 <- procspec(fakedata1, "max")
fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
fakedata2 <- procspec(fakedata2, "sum")
fakedata2 <- procspec(fakedata2, "min")

# converting reflectance to percentage
fakedata1[, -1] <- fakedata1[, -1] * 100
fakedata2[, -1] <- fakedata2[, -1] / max(fakedata2[, -1]) * 100

# combining and converting to rspec
fakedata.c <- data.frame(wl = 300:700, fakedata1[, -1], fakedata2[, -1])
fakedata.c <- as.rspec(fakedata.c)

# segment classification analysis
seg.vis <- vismodel(fakedata.c, visual = "segment", achromatic = "all")
seg.fdc <- colspace(seg.vis, space = "segment")

# plot results
plot(seg.fdc, col = spec2rgb(fakedata.c))

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  head(coldist(tetra.flowers))

## ------------------------------------------------------------------------
# Model flower colours according to a honeybee
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE, achro = "l", bkg = "green")
hex.flowers <- colspace(vis.flowers, space = "hexagon")

# Estimate color distances. No need to specify relative receptor densities, noise etc.,
# which only apply in the case of receptor-noise modelling
dist.flowers <- coldist(hex.flowers)
head(dist.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.align='center', fig.cap="_Visual system of a pretend mantis shrimp with 10 cones_"----
# Create an arbitrary visual phenotype with 10 photoreceptors
fakemantisshrimp <- sensmodel(c(325, 350, 400, 425, 450, 500, 550, 600, 650, 700), beta = FALSE, integrate = FALSE)

# Convert to percentages, just to color the plot
fakemantisshrimp.colors <- fakemantisshrimp * 100
fakemantisshrimp.colors[, "wl"] <- fakemantisshrimp[, "wl"]

plot(fakemantisshrimp, col = spec2rgb(fakemantisshrimp.colors), lwd = 2, ylab = "Absorbance")

# Run visual model and calculate color distances
vm.fms <- vismodel(flowers, visual = fakemantisshrimp, relative = FALSE, achro = FALSE)

JND.fms <- coldist(vm.fms, n = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))

head(JND.fms)

## ---- eval = FALSE-------------------------------------------------------
#  butterflies <- getimg("~/pavo/vignette_data/images/")
#  # 2  files found; importing images
#  # |================================================================================| 100%, ETA 00:00

## ---- echo=FALSE, eval=TRUE, results='hide', include=FALSE---------------
# specs <- getspec(system.file("extdata", package = "pavo"), ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
butterflies <- getimg(system.file("testdata/images/", package = 'pavo'))

## ------------------------------------------------------------------------
is.rimg(butterflies)
str(butterflies[[1]])
str(butterflies[[2]])

## ------------------------------------------------------------------------
fakeimg <- array(c(
  matrix(c(1, 1, 0, 0), nrow = 12, ncol = 8),
  matrix(c(0, 0, 0, 0), nrow = 12, ncol = 8),
  matrix(c(0, 0, 1, 1), nrow = 12, ncol = 8)),
  dim = c(12, 8, 3)
)
fake_rimg <- as.rimg(fakeimg)
is.rimg(fake_rimg)
str(fake_rimg)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.align='center', fig.cap="_Raw images of our butterflies_"----

# Note the plot titles are taken from the file names, and can be overridden.
plot(butterflies[[1]])

plot(butterflies[[2]])


## ---- eval = FALSE-------------------------------------------------------
#  butterflies <- procimg(butterflies, scaledist = 100)

## ---- eval = FALSE-------------------------------------------------------
#  butterflies <- procimg(butterflies, outline = TRUE, iterations = 1)

## ------------------------------------------------------------------------
set.seed(5)
butterflies_class <- classify(butterflies, kcols = c(4, 3))

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.align='center', fig.cap="_The k-means classified images of our butterflies, along with their identified color palettes_"----

# Note that we could simply feed the list of images to summary, rather than 
# specifying individual images, and they would progress automatically 
# with user input.

summary(butterflies_class[[2]], plot = TRUE)


## ---- eval = FALSE-------------------------------------------------------
#  # Automatic classification.
#  butterflies_class <- classify(butterflies, kcols = c(4, 3))
#  
#  # Automatic classification using a reference image
#  butterflies_class <- classify(butterflies, kcols = c(4, 3))
#  
#  # Classification using interactively-specified centres for each image, with no
#  # need to specify kcols (since it will be inferred from the numbers of colours selected)
#  butterflies_class <- classify(butterflies, interactive = TRUE)
#  
#  # Classification using interactively-specified centres for the single reference image
#  # (here, the first in the list).
#  butterflies_class <- classify(butterflies, refID = 1, interactive = TRUE)

## ------------------------------------------------------------------------
butterflies_adj <- adjacent(butterflies_class, xscale = 200, xpts = 200, bkgID = 1)

head(butterflies_adj)

## ------------------------------------------------------------------------

# Create a fake matrix of pairwise color- and luminance distances between all 
# color patten elements, as might be attained through visual modelling of spectral data.
distances <- data.frame(c1 = c(1, 1, 2),
                        c2 = c(2, 3, 3),
                        dS = c(10.6, 5.1, 4.4),
                        dL = c(1.1, 2.5, 3.2))

# Take a look
distances

# And our fake hue angles (in radians), saturation, and luminance values, for each
# color pattern element
hsl_vals <- data.frame(patch = 1:3,
                       hue = c(1.2, 2.2, 1.6),
                       lum = c(10, 5, 7),
                       sat = c(3.5, 1.1, 6.3))

# Take a look
hsl_vals

# Now feed this information into the adjacency analysis using the less-colorful 
# of our two images, for convenience (though this could be readily extended to 
# include a list of images along with a list of distances and hsl values)
adjacent(butterflies_class[[2]], xscale = 200, xpts = 200, bkgID = 1, coldists = distances, hsl = hsl_vals)


