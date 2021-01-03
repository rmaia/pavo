## ----include = FALSE----------------------------------------------------------
knitr::knit_hooks$set(fig = knitr::hook_pngquant)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.show = "hold"
)

## ---- warning=FALSE, results='hide', message=FALSE----------------------------
# Load the package, and set a global random-number seed for the reproducible generation of fake data later on.
library(pavo)
set.seed(1612217)

## -----------------------------------------------------------------------------
specs <- readRDS(system.file("extdata/specsdata.rds", package = "pavo"))
mspecs <- aggspec(specs, by = 3, FUN = mean)
spp <- gsub('\\.[0-9].*$', '', names(mspecs))[-1]
sppspec <- aggspec(mspecs, by = spp, FUN = mean)
spec.sm <- procspec(sppspec, opt = "smooth", span = 0.2)

## ---- results='hide'----------------------------------------------------------
summary(spec.sm)

## ---- results='hide'----------------------------------------------------------
summary(spec.sm, subset = TRUE)

## ---- results='hide'----------------------------------------------------------
# Extract only brightness variables
summary(spec.sm, subset = c('B1', 'B2', 'B3'))

## ---- fig=TRUE, include=TRUE, fig.cap="Plots from `peakshape`", fig.show="hold", out.width="30%"----
peakshape(spec.sm, plot = TRUE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Plot from `peakshape`, setting the wavelength limits to 300 and 500 nm"----
peakshape(spec.sm, select = 2, lim = c(300, 500), plot = TRUE)

## ----echo=TRUE----------------------------------------------------------------
musca_sense <- sensdata(visual = "musca", achromatic = "md.r1")
head(musca_sense)

## ----echo = FALSE, results = 'asis'-------------------------------------------
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

## -----------------------------------------------------------------------------
vismod1 <- vismodel(sppspec,
  visual = "avg.uv", achromatic = "bt.dc",
  illum = "D65", relative = FALSE
)

## -----------------------------------------------------------------------------
summary(vismod1)

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=8.5, fig.height=5, fig.cap="Plots of species mean reflectance curves with corresponding relative usml cone stimulations (insets)."----
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 6), oma = c(3, 3, 0, 0))
layout(rbind(c(2, 1, 4 , 3, 6 , 5),
             c(1, 1, 3 , 3, 5 , 5), 
             c(8, 7, 10, 9, 12, 11),
             c(7, 7, 9 , 9, 11, 11)))

sppspecol <- spec2rgb(sppspec)

for (i in 1:6) {
  par(mar = c(2, 2, 2, 2))
  plot(sppspec, select = i+1, col = sppspecol, lwd = 3, ylim = c(0, 100))
  par(mar = c(4.1, 2.5, 2.5, 2))
  barplot(as.matrix(vismod1[i, 1:4]), yaxt = "n", col = "black")
}

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)
par(oldpar)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Idealized dichromat photoreceptors created using `sensmodel`."----
idealizeddichromat <- sensmodel(c(350, 650))
plot(idealizeddichromat, col = spec2rgb(idealizeddichromat), ylab = "Absorbance")

## ---- results='hide'----------------------------------------------------------
vismod.idi <- vismodel(sppspec, visual = idealizeddichromat, relative = FALSE)
vismod.idi

## ---- message = FALSE---------------------------------------------------------
coldist(vismod1,
  noise = "neural", achromatic = TRUE, n = c(1, 2, 2, 4),
  weber = 0.1, weber.achro = 0.1
)
coldist(vismod.idi, n = c(1, 2), weber = 0.1)

## ---- message=FALSE-----------------------------------------------------------
coldist(vismod1, subset = 'cardinal')

## ---- message=FALSE-----------------------------------------------------------
coldist(vismod1, subset = c('cardinal', 'jacana'))

## ---- message=FALSE, warning=FALSE--------------------------------------------

# Load the data
data(sicalis)

# Construct a model using an avian viewer
sicmod <- vismodel(sicalis, visual = 'avg.uv', relative = FALSE)

# Create a grouping variable to group by body region,
# informed by the spectral data names
regions <- substr(rownames(sicmod), 6, 6)

# Estimate bootstrapped colour-distances
sicdist <- bootcoldist(sicmod, by = regions, n = c(1, 2, 2, 4), weber = 0.05)

# Take a look at the resulting pairwise estimates 
sicdist

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="The bootstrapped colour distances between the throat, chest, and breast regions of male Sicalis citrina."----
plot(sicdist[, 1], 
     ylim = c(0, 20), 
     pch = 21, 
     bg = 1, 
     cex = 2, 
     xaxt = 'n', 
     xlab = 'Centroid comparison', 
     ylab = 'Chromatic contrast (dS)')
axis(1, at = 1:3, labels = rownames(sicdist))
segments(1:3, sicdist[, 2], 1:3, sicdist[, 3], lwd = 2)  # Add CI's
abline(h = 1, lty = 3, lwd = 2)  # Add a 'threshold' line at dS = 1

## ---- message=FALSE-----------------------------------------------------------
fakedata1 <- vapply(
  seq(100, 500, by = 20),
  function(x) rowSums(cbind(
      dnorm(300:700, x, 30),
      dnorm(300:700, x + 400, 30)
    )),
    numeric(401)
)

# Creating idealized specs with varying saturation
fakedata2 <- vapply(
  c(500, 300, 150, 105, 75, 55, 40, 30),
  function(x) dnorm(300:700, 550, x),
  numeric(401)
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

## ---- message=FALSE-----------------------------------------------------------
# Visual model and colour distances
fakedata.vm <- vismodel(fakedata.c, relative = FALSE, achromatic = 'all')
fakedata.cd <- coldist(fakedata.vm,
  noise = "neural", n = c(1, 2, 2, 4),
  weber = 0.1, achromatic = TRUE
)

# Converting to Cartesian coordinates
fakedata.cc <- jnd2xyz(fakedata.cd, ref1 = "l", axis1 = c(1, 0, 0), ref2 = NULL)
head(fakedata.cc)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Spectral data in a receptor noise-corrected colourspace"----
plot(fakedata.cc, theta = 55, phi = 25, col = spec2rgb(fakedata.c))

## -----------------------------------------------------------------------------
data(flowers)

head(flowers[1:4])

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Flowers in a dichromatic colourspace, as modelled according to a canid visual system."----
plot(di.flowers, col = spec2rgb(flowers))

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'fi', scale = 10000)

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=4, fig.height=4, fig.cap="Floral reflectance in a Maxwell triangle, considering a honeybee visual system."----
plot(tri.flowers, pch = 21, bg = spec2rgb(flowers))

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "bluetit", qcatch = "fi", scale = 10000)

tetra.flowers <- colspace(vis.flowers, space = "tcs")

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.height=5, fig.width=5, fig.cap="Flowers in a tetrahedral colourspace modelled using the visual phenotype of the blue tit. Point size is used to force perspective"----
plot(tetra.flowers, pch = 21, bg = spec2rgb(flowers), perspective = TRUE, range = c(1, 2), cex = 0.5)

## ---- fig=TRUE, include=TRUE, fig.cap="Flowers in a tetrahedral colourspace, with varied orientations and perspectives, modelled using the visual phenotype of the blue tit.", fig.show="hold", out.width="45%"----
plot(tetra.flowers, pch = 21, bg = spec2rgb(flowers))
axistetra(x = 0, y = 1.8)
plot(tetra.flowers, theta = 110, phi = 10, pch = 21, bg = spec2rgb(flowers))
axistetra(x = 0, y = 1.8)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Projection plot from a tetrahedral colour space."----
projplot(tetra.flowers, pch = 20, col = spec2rgb(flowers))

## -----------------------------------------------------------------------------
data(sicalis)

## ---- plot = TRUE, fig.show='hold', out.width='40%'---------------------------
tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")
voloverlap(tcs.sicalis.T, tcs.sicalis.B, plot = TRUE)
voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE)

## -----------------------------------------------------------------------------
summary(tetra.flowers)

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achromatic = 'l', bkg = 'green')

## -----------------------------------------------------------------------------
hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers as modelled in the hymenopteran colour hexagon of Chittka (1992), overlain with coarse bee-hue sectors."----
plot(hex.flowers, sectors = 'coarse', pch = 21, bg = spec2rgb(flowers))

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE, bkg = "green")

coc.flowers <- colspace(vis.flowers, space = "coc")

head(coc.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers in the colour-opponent-coding space of Backhaus (1991), as modelling according to the honeybee."----
plot(coc.flowers, pch = 21, bg = spec2rgb(flowers), yaxt = "n")

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE, achromatic = 'none')

## -----------------------------------------------------------------------------
ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
head(ciexyz.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Floral reflectance in the CIEXYZ human visual model. Note that this space is not perceptually calibrated, so we cannot make inferences about the similarity or differences of colours based on their relative location."----
plot(ciexyz.flowers, pch = 21, bg = spec2rgb(flowers))

## -----------------------------------------------------------------------------
cielab.flowers <- colspace(vis.flowers, space = 'cielab')
head(cielab.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Floral reflectance spectra represented in the CIELab model of human colour sensation."----
plot(cielab.flowers, pch = 21, bg = spec2rgb(flowers))

## -----------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achromatic = 'none', relative = TRUE)

## -----------------------------------------------------------------------------
cat.flowers <- colspace(vis.flowers, space = 'categorical')

head(cat.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Flowers in the categorical colourspace of Troje (1993)."----
plot(cat.flowers, pch = 21, bg = spec2rgb(flowers))

## ---- fig=TRUE, include=TRUE, results = 'hide', fig.width=4, fig.height=4, fig.cap="Idealized reflectance spectra and their projection on the axes of segment classification"----
fakedata1 <- vapply(
  seq(100, 500, by = 20),
  function(x) rowSums(cbind(
      dnorm(300:700, x, 30),
      dnorm(300:700, x + 400, 30)
    )), numeric(401)
)

# creating idealized specs with varying saturation
fakedata2 <- vapply(
  c(500, 300, 150, 105, 75, 55, 40, 30),
  function(x) dnorm(300:700, 550, x),
  numeric(401)
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

## -----------------------------------------------------------------------------
head(coldist(tetra.flowers))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
butterflies <- getimg(system.file("testdata/images/butterflies/", package = 'pavo'))

## -----------------------------------------------------------------------------
butterflies_class <- classify(butterflies, kcols = c(4, 3))

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="The k-means classified images of our butterflies, along with their identified colour palettes"----

# Note that we could simply feed the list of images to summary, rather than
# specifying individual images, and they would progress automatically
# with user input.
summary(butterflies_class[[2]], plot = TRUE)


## -----------------------------------------------------------------------------
# Automatic classification.
butterflies_class <- classify(butterflies, kcols = c(4, 3))

# Automatic classification using a reference image, identified by name.
butterflies_class <- classify(butterflies, refID = 'h_melpomene', kcols = 3)

## ---- eval = FALSE------------------------------------------------------------
#  # Classification using interactively-specified centres for each image, with no
#  # need to specify kcols (since it will be inferred from the numbers of colours selected)
#  butterflies_class <- classify(butterflies, interactive = TRUE)
#  
#  # Classification using interactively-specified centres for the single reference image
#  # (here, the first in the list). We could also specify reference image using it's name,
#  # as above.
#  butterflies_class <- classify(butterflies, refID = 1, interactive = TRUE)

## -----------------------------------------------------------------------------

# Load up our images
butterflies <- getimg(system.file("testdata/images/", package = 'pavo'))

# Automatically classify discrete colour patches (as confirmed by spectral modelling)
# using k-means clustering, with values of 'k' that have been validated a priori.
butterflies_class <- classify(butterflies, kcols = c(4, 3))

# Run the adjacency analysis, subsampling the image in a 200x200 grid, and excluding
# the white background.
butterflies_adj <- adjacent(butterflies_class, xscale = 200, xpts = 200, bkgID = 1)

# Take a look
head(butterflies_adj)

