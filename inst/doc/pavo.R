## ----include = FALSE-----------------------------------------------------
# Do not use partial matching
options(
   warnPartialMatchDollar = TRUE,
   warnPartialMatchArgs = TRUE,
   warnPartialMatchAttr = TRUE
)

## ---- echo = FALSE, fig.cap = "A non-exhaustive overview of the colour-pattern analysis workflow in pavo, as of version 2.0, displaying some key functions at each stage.", out.width = '70%', dpi = 72----
knitr::include_graphics('fig/workflow.png')

## ---- warning=FALSE, results='hide', message=FALSE-----------------------
# Load the package, and set a global random-number seed for the reproducible generation of fake data later on.
library(pavo)
set.seed(1612217)

## ---- echo=TRUE, eval=FALSE, results='hide', include=TRUE----------------
#  specs <- getspec("~/pavo/vignette_data/", ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
#  # 213  files found; importing spectra
#  # |================================================================================| 100%, ETA 00:00

## ------------------------------------------------------------------------
specs <- readRDS(system.file("extdata/specsdata.rda", package = "pavo"))

## ------------------------------------------------------------------------
specs[1:10, 1:4]
dim(specs) # the data set has 213 spectra, from 300 to 700 nm, plus a 'wl' column

## ------------------------------------------------------------------------
is.rspec(specs)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
head(as.rspec(fakedat, whichwl = 3))

## ---- fig.height=3, fig.width=4------------------------------------------
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 500))

plot(refl1 ~ wl, type = "l", data = fakedat.new2)

## ---- fig.height=3, fig.width=4------------------------------------------
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 1000))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = "l")

## ------------------------------------------------------------------------
specs.tanager1 <- subset(specs, "tanager")

head(specs.tanager1)[1:5]

## ------------------------------------------------------------------------
# extract first component of filenames containing species names
spp <- do.call(rbind, strsplit(names(specs), "\\."))[, 1]

# subset
specs.tanager2 <- subset(specs, subset = spp == "tanager")

# compare subsetting methods
all.equal(specs.tanager1, specs.tanager2)

## ------------------------------------------------------------------------
specs.tanager <- subset(specs, "tanager")
specs.parakeet <- subset(specs, "parakeet")
specs.new <- merge(specs.tanager, specs.parakeet)

## ---- label=explorespecfig, fig=TRUE, include=TRUE, fig.width=6, fig.height=4.5, fig.cap="Result from `explorespec`, showing the three measurements for each individual cardinal in separate panels"----
# 36 spectra plus the first (wl) column
explorespec(specs[, 1:37], by = 3, lwd = 2)

## ------------------------------------------------------------------------
mspecs <- aggspec(specs, by = 3, FUN = mean)
mspecs[1:5, 1:4]
dim(mspecs) # data now has 71 spectra, one for each individual, and the 'wl' column

## ------------------------------------------------------------------------
# create a vector with species identity names
spp <- gsub('\\.[0-9].*$', '', names(mspecs))[-1]
table(spp)

## ---- label=exploresppmeans, fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.cap="Result from `explorespec` for species means"----
sppspec <- aggspec(mspecs, by = spp, FUN = mean)
round(sppspec[1:5, ], 2)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4.5----------------
plotsmooth(sppspec, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Result for raw (grey line) and smoothed (red line) reflectance data for the parakeet"----
spec.sm <- procspec(sppspec, opt = "smooth", span = 0.2)
plot(sppspec[, 5] ~ sppspec[, 1],
  type = "l", lwd = 10, col = "grey",
  xlab = "Wavelength (nm)", ylab = "Reflectance (%)"
)
lines(spec.sm[, 5] ~ sppspec[, 1], col = "red", lwd = 2)

## ---- results='hide'-----------------------------------------------------
# Run some different normalisations
specs.max <- procspec(sppspec, opt = "max")
specs.min <- procspec(sppspec, opt = "min")
specs.str <- procspec(sppspec, opt = c("min", "max")) # multiple options

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=3, fig.cap="Results for min (left), max (centre), and both normalisations (right)"----
# Plot results
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2), oma = c(3, 3, 0, 0))

plot(specs.min[, 5] ~ c(300:700), xlab = "", ylab = "", type = "l")
abline(h = 0, lty = 2)

plot(specs.max[, 5] ~ c(300:700), ylim = c(0, 1), xlab = "", ylab = "", type = "l")
abline(h = c(0, 1), lty = 2)

plot(specs.str[, 5] ~ c(300:700), type = "l", xlab = "", ylab = "")
abline(h = c(0, 1), lty = 2)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Normalised reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- results='hide'-----------------------------------------------------
# PCA analysis
spec.bin <- procspec(sppspec, opt = c("bin", "center"))
head(spec.bin)
spec.bin <- t(spec.bin) # transpose so wavelength are variables for the PCA
colnames(spec.bin) <- spec.bin[1, ]  # names variables as wavelength bins
spec.bin <- spec.bin[-1, ]  # remove 'wl' column
pca1 <- prcomp(spec.bin, scale. = TRUE)

## ------------------------------------------------------------------------
summary(pca1)

## ---- fig=TRUE, include=TRUE, fig.width=7, fig.height=3, fig.cap="Plot of PC1 loading versus wavelength (left) and species mean spectra sorted vertically from lowest to highest PC1 value (right; values on right hand axis are column identities)."----

# Generate colours from spectra
colr <- spec2rgb(sppspec)
wls <- as.numeric(colnames(spec.bin))

# Rank specs by PC1
sel <- rank(pca1$x[, 1])
sel <- match(names(sort(sel)), names(sppspec))

# Plot results
par(mfrow = c(1, 2), mar = c(2, 4, 2, 5), oma = c(2, 0, 0, 0))
plot(pca1$rotation[, 1] ~ wls, type = "l", ylab = "PC1 loading")
abline(h = 0, lty = 2)
plot(sppspec, select = sel, labels.stack = names(sppspec)[sel], type = "s", col = colr)
mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)

## ---- results='hide'-----------------------------------------------------
# Create a duplicate spectrum and add some negative values
refl <- sppspec[, 7] - 20
testspecs <- as.rspec(cbind(c(300:700), refl))

# Apply two different processing options
testspecs.fix1 <- procspec(testspecs, fixneg = "addmin")
testspecs.fix2 <- procspec(testspecs, fixneg = "zero")

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.cap="Plots showing original reflectance curve including negative values (left) and two processed curves using `fixneg = addmin` (top right) and `fixneg = zero` (bottom right)."----
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

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Overlay plot of the teal angle-dependent reflectance with colours of each curve being an approximation of the perceived colour."----
par(mar = c(4, 4, 2, 2))
data(teal)
plot(teal, type = "o", col = spec2rgb(teal))

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=5.5, fig.cap="Stacked plot of the raw (left) and normalized (right) teal angle-dependent reflectance"----
teal.norm <- procspec(teal, opt = c("min", "max"))
par(mfrow = c(1, 2), mar = c(2, 2, 2, 2), oma = c(2, 2, 0, 0))

plot(teal, type = "s", col = spec2rgb(teal))
plot(teal.norm, type = "s", col = spec2rgb(teal))

mtext("Wavelength (nm)", side = 1, outer = T, line = 1)
mtext("Cumulative reflectance (A.U.)", side = 2, outer = T, line = 1)

## ------------------------------------------------------------------------
angles <- seq(15, 70, by = 5)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Heatmap plot for angle-resolved reflectance measurements of the green-winged teal."----

# Smooth the spectral data
teal.sm <- procspec(teal, opt = c("smooth"))

# Plot it as a heatmap
plot(teal.sm,
  type = "h", varying = angles,
  ylab = expression(paste("Incident angle (", degree, ")")),
  las = 1, useRaster = TRUE
)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4, fig.cap="Example plots created using `aggplot`. Left: using median, standard deviation, and coloured lines. Right: using mean, standard error, and greyscale"----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2), oma = c(2, 0, 0, 0))

# Plot using median and standard deviation, default colours
aggplot(mspecs, spp, 
        FUN.center = median, 
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE)

# Plot using mean and standard error, in greyscale
aggplot(mspecs, spp,
        ylim = c(0, 70),
        FUN.error = function(x) sd(x) / sqrt(length(x)),
        lcol = 1, shadecol = "grey", alpha = 0.7)

## ---- results='hide'-----------------------------------------------------
summary(spec.sm)

## ---- results='hide'-----------------------------------------------------
summary(spec.sm, subset = TRUE)

## ---- results='hide'-----------------------------------------------------
# Extract only brightness variables
summary(spec.sm, subset = c('B1', 'B2', 'B3'))

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Plots from `peakshape`"----
par(mfrow = c(2, 3))
peakshape(spec.sm, plot = TRUE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Plot from `peakshape`, setting the wavelength limits to 300 and 500 nm"----
peakshape(spec.sm, select = 2, lim = c(300, 500), plot = TRUE)

## ----echo=TRUE-----------------------------------------------------------
musca_sense <- sensdata(visual = "musca", achromatic = "md.r1")
head(musca_sense)

## ----echo = FALSE, results = 'asis'--------------------------------------
vistab <- data.frame(phenotype = c("avg.uv", "avg.v", "bluetit", "star", "pfowl", "apis", "ctenophorus", "canis", "musca", "cie2",
                                   "cie10", "segment", "habronattus", "rhinecanthus"),
                     description = c("average ultraviolet-sensitive avian (tetrachromat)",
                                     "average violet-sensitive avian (tetrachromat)",
                                     "The blue tit _Cyanistes caeruleus_ (tetrachromat)",
                                     "The starling _Sturnus vulgaris_ (tetrachromat)",
                                     "The peafowl _Pavo cristatus_ (tetrachromat)",
                                     "The honeybee _Apis mellifera_ (trichromat)",
                                     "The ornate dragon lizard _Ctenophorus ornatus_ (trichromat)",
                                     "The canid _Canis familiaris_ (dichromat)",
                                     "The housefly _Musca domestica_ (tetrachromat)",
                                     "2-degree colour matching functions for CIE models of human colour vision (trichromat)",
                                     "10-degree colour matching functions for CIE models of human colour vision (trichromat)",
                                     "A generic 'viewer' with broad sensitivities for use in the segment analysis of Endler (1990) (tetrachromat)",
                                     "The jumping spider _Habronattus pyrrithrix_ (trichromat)",
                                     "The triggerfish _Rhinecanthus aculeatus_ (trichromat)"))
knitr::kable(vistab, caption = "Built-in visual phenotypes available in pavo")

## ---- results='hide'-----------------------------------------------------
vismod1 <- vismodel(sppspec,
  visual = "avg.uv", achromatic = "bt.dc",
  illum = "D65", relative = FALSE
)
vismod1

## ------------------------------------------------------------------------
summary(vismod1)

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=6, fig.height=5, fig.cap="Plots of species mean reflectance curves with corresponding relative usml cone stimulations (insets)."----
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

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Idealized dichromat photoreceptors created using `sensmodel`."----
idealizeddichromat <- sensmodel(c(350, 650))
plot(idealizeddichromat, col = spec2rgb(idealizeddichromat), ylab = "Absorbance")

## ---- results='hide'-----------------------------------------------------
vismod.idi <- vismodel(sppspec, visual = idealizeddichromat, relative = FALSE)
vismod.idi

## ------------------------------------------------------------------------
coldist(vismod1,
  noise = "neural", achromatic = TRUE, n = c(1, 2, 2, 4),
  weber = 0.1, weber.achro = 0.1
)
coldist(vismod.idi, n = c(1, 2), weber = 0.1)

## ---- results = 'hide'---------------------------------------------------
coldist(vismod1, subset = 'cardinal')

## ---- results='hide'-----------------------------------------------------
coldist(vismod1, subset = c('cardinal', 'jacana'))

## ------------------------------------------------------------------------
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
# Visual model and colour distances
fakedata.vm <- vismodel(fakedata.c, relative = FALSE, achromatic = TRUE)
fakedata.cd <- coldist(fakedata.vm,
  noise = "neural", n = c(1, 2, 2, 4),
  weber = 0.1, achromatic = TRUE
)

# Converting to Cartesian coordinates
fakedata.cc <- jnd2xyz(fakedata.cd, ref1 = "l", axis1 = c(1, 0, 0), ref2 = NULL)
head(fakedata.cc)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Spectral data in a receptor noise-corrected colourspace"----
plot(fakedata.cc, theta = 55, phi = 25, col = spec2rgb(fakedata.c))

## ------------------------------------------------------------------------
data(flowers)

head(flowers[1:4])

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Flowers in a dichromatic colourspace, as modelled according to a canid visual system."----
plot(di.flowers, col = spec2rgb(flowers))

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'fi', scale = 10000)

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Floral reflectance in a Maxwell triangle, considering a honeybee visual system."----
plot(tri.flowers, pch = 21, bg = spec2rgb(flowers))

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "bluetit", qcatch = "fi", scale = 10000)

tetra.flowers <- colspace(vis.flowers, space = "tcs")

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.height=5, fig.width=5, fig.cap="Flowers in a tetrahedral colourspace modelled using the visual phenotype of the blue tit. Point size is used to force perspective"----
plot(tetra.flowers, pch = 21, bg = spec2rgb(flowers), perspective = TRUE, range = c(1, 2), cex = 0.5)

## ---- fig=TRUE, include=TRUE, fig.height=4, fig.width=6, fig.cap="Flowers in a tetrahedral colourspace, with varied orientations and perspectives, modelled using the visual phenotype of the blue tit."----
par(mfrow = c(1, 2), pty = "s")
plot(tetra.flowers, pch = 21, bg = spec2rgb(flowers))
axistetra(x = 0, y = 1.8)
plot(tetra.flowers, theta = 110, phi = 10, pch = 21, bg = spec2rgb(flowers))
axistetra(x = 0, y = 1.8)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Projection plot from a tetrahedral colour space."----
projplot(tetra.flowers, pch = 20, col = spec2rgb(flowers))

## ------------------------------------------------------------------------
data(sicalis)

## ------------------------------------------------------------------------
par(mfrow = c(1, 2), pty = "s")
tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")
voloverlap(tcs.sicalis.T, tcs.sicalis.B, plot = TRUE)
voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE)

## ------------------------------------------------------------------------
summary(tetra.flowers)

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achromatic = 'l', bkg = 'green')

## ------------------------------------------------------------------------
hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers as modelled in the hymenopteran colour hexagon of Chittka (1992), overlain with coarse bee-hue sectors."----
plot(hex.flowers, sectors = 'coarse', pch = 21, bg = spec2rgb(flowers))

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE, bkg = "green")

coc.flowers <- colspace(vis.flowers, space = "coc")

head(coc.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers in the colour-opponent-coding space of Backhaus (1991), as modelling according to the honeybee."----
plot(coc.flowers, pch = 21, bg = spec2rgb(flowers), yaxt = "n")

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE, achromatic = 'none')

## ------------------------------------------------------------------------
ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
head(ciexyz.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Floral reflectance in the CIEXYZ human visual model. Note that this space is not perceptually calibrated, so we cannot make inferences about the similarity or differences of colours based on their relative location."----
plot(ciexyz.flowers, pch = 21, bg = spec2rgb(flowers))

## ------------------------------------------------------------------------
cielab.flowers <- colspace(vis.flowers, space = 'cielab')
head(cielab.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Floral reflectance spectra represented in the CIELab model of human colour sensation."----
plot(cielab.flowers, pch = 21, bg = spec2rgb(flowers))

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achromatic = 'none', relative = TRUE)

## ------------------------------------------------------------------------
cat.flowers <- colspace(vis.flowers, space = 'categorical')

head(cat.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers in the categorical colourspace of Troje (1993)."----
plot(cat.flowers, pch = 21, bg = spec2rgb(flowers))

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=4, fig.height=4, fig.cap="Idealized reflectance spectra and their projection on the axes of segment classification"----
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

## ------------------------------------------------------------------------
head(coldist(tetra.flowers))

## ------------------------------------------------------------------------
# Model flower colours according to a honeybee
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE, achromatic = "l", bkg = "green")
hex.flowers <- colspace(vis.flowers, space = "hexagon")

# Estimate colour distances. No need to specify relative receptor densities, noise etc.,
# which only apply in the case of receptor-noise modelling
dist.flowers <- coldist(hex.flowers)
head(dist.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Visual system of a pretend mantis shrimp with 10 cones"----
# Create an arbitrary visual phenotype with 10 photoreceptors
fakemantisshrimp <- sensmodel(c(325, 350, 400, 425, 450, 500, 550, 600, 650, 700), beta = FALSE, integrate = FALSE)

# Convert to percentages, just to colour the plot
fakemantisshrimp.colours <- fakemantisshrimp * 100
fakemantisshrimp.colours[, "wl"] <- fakemantisshrimp[, "wl"]

plot(fakemantisshrimp, col = spec2rgb(fakemantisshrimp.colours), lwd = 2, ylab = "Absorbance")

# Run visual model and calculate colour distances
vm.fms <- vismodel(flowers, visual = fakemantisshrimp, relative = FALSE, achromatic = FALSE)

JND.fms <- coldist(vm.fms, n = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))

head(JND.fms)

## ------------------------------------------------------------------------
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

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Raw images of our butterflies"----

# Note the plot titles are taken from the file names, and can be overridden.
plot(butterflies[[1]])


## ---- eval = FALSE-------------------------------------------------------
#  # Interactively specify the real-world scale of the image. Here 100 mm.
#  butterflies <- procimg(butterflies, scaledist = 100)

## ---- eval=FALSE---------------------------------------------------------
#  # Interactively specify a smoothed polygon around the focal objects
#  butterflies <- procimg(butterflies, outline = TRUE, iterations = 1)

## ------------------------------------------------------------------------
butterflies_class <- classify(butterflies, kcols = c(4, 3))

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="The k-means classified images of our butterflies, along with their identified colour palettes"----

# Note that we could simply feed the list of images to summary, rather than
# specifying individual images, and they would progress automatically
# with user input.

summary(butterflies_class[[2]], plot = TRUE)


## ------------------------------------------------------------------------
# Automatic classification.
butterflies_class <- classify(butterflies, kcols = c(4, 3))

# Automatic classification using a reference image
butterflies_class <- classify(butterflies, , refID = 1, kcols = c(4, 3))

## ---- eval = FALSE-------------------------------------------------------
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
# Create a fake matrix of pairwise colour- and luminance distances between all
# colour patten elements, as might be attained through visual modelling of spectral data.
distances <- data.frame(c1 = c(1, 1, 2),
                        c2 = c(2, 3, 3),
                        dS = c(10.6, 5.1, 4.4),
                        dL = c(1.1, 2.5, 3.2))

# Take a look
distances

# And our fake hue angles (in radians), saturation, and luminance values, for each
# colour pattern element
hsl_vals <- data.frame(patch = 1:3,
                       hue = c(1.2, 2.2, 1.6),
                       lum = c(10, 5, 7),
                       sat = c(3.5, 1.1, 6.3))

# Take a look
hsl_vals

# Now feed this information into the adjacency analysis using the less-colourful
# of our two images, for convenience (though this could be readily extended to
# include a list of images along with a list of distances and hsl values)
adjacent(butterflies_class[[2]], xscale = 200, xpts = 200, bkgID = 1, coldists = distances, hsl = hsl_vals)

