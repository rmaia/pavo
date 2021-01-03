## ----include = FALSE----------------------------------------------------------
# Do not use partial matching
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

## ---- echo=TRUE, eval=FALSE, results='hide', include=TRUE---------------------
#  specs <- getspec("~/pavo/data_external/vignette", ext = "ttt", decimal = ",", subdir = TRUE, subdir.names = FALSE)
#  # 213  files found; importing spectra
#  # |================================================================================| 100%, ETA 00:00

## -----------------------------------------------------------------------------
specs <- readRDS(system.file("extdata/specsdata.rds", package = "pavo"))

## -----------------------------------------------------------------------------
specs[1:10, 1:4]
dim(specs) # the data set has 213 spectra, from 300 to 700 nm, plus a 'wl' column

## -----------------------------------------------------------------------------
is.rspec(specs)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
head(as.rspec(fakedat, whichwl = 'wavelength'))

## ---- fig.height=3, fig.width=4, fig=TRUE-------------------------------------
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 500))

plot(refl1 ~ wl, type = "l", data = fakedat.new2)

## ---- fig.height=3, fig.width=4, fig = TRUE-----------------------------------
fakedat.new2 <- as.rspec(fakedat, lim = c(300, 1000))

plot(fakedat.new2[, 2] ~ fakedat.new2[, 1], type = "l")

## -----------------------------------------------------------------------------
specs.tanager1 <- subset(specs, "tanager")

head(specs.tanager1)[1:5]

## -----------------------------------------------------------------------------
# extract first component of filenames containing species names
spp <- do.call(rbind, strsplit(names(specs), "\\."))[, 1]

# subset
specs.tanager2 <- subset(specs, subset = spp == "tanager")

# compare subsetting methods
all.equal(specs.tanager1, specs.tanager2)

## -----------------------------------------------------------------------------
specs.tanager <- subset(specs, "tanager")
specs.parakeet <- subset(specs, "parakeet")
specs.new <- merge(specs.tanager, specs.parakeet)

## ---- label=explorespecfig, fig=TRUE, include=TRUE, fig.width=6, fig.height=4.5, fig.cap="Result from `explorespec`, showing the three measurements for each individual cardinal in separate panels"----
# 36 spectra plus the first (wl) column
explorespec(specs[, 1:37], by = 3, lwd = 2)

## -----------------------------------------------------------------------------
mspecs <- aggspec(specs, by = 3, FUN = mean)
mspecs[1:5, 1:4]
dim(mspecs) # data now has 71 spectra, one for each individual, and the 'wl' column

## -----------------------------------------------------------------------------
# create a vector with species identity names
spp <- gsub('\\.[0-9].*$', '', names(mspecs))[-1]
table(spp)

## ---- label=exploresppmeans, fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.cap="Result from `explorespec` for species means"----
sppspec <- aggspec(mspecs, by = spp, FUN = mean)
round(sppspec[1:5, ], 2)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4.5---------------------
plotsmooth(sppspec, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Result for raw (grey line) and smoothed (red line) reflectance data for the parakeet"----
spec.sm <- procspec(sppspec, opt = "smooth", span = 0.2)
plot(sppspec[, 5] ~ sppspec[, 1],
  type = "l", lwd = 10, col = "grey",
  xlab = "Wavelength (nm)", ylab = "Reflectance (%)"
)
lines(spec.sm[, 5] ~ sppspec[, 1], col = "red", lwd = 2)

## ---- results='hide'----------------------------------------------------------
# Run some different normalisations
specs.max <- procspec(sppspec, opt = "max")
specs.min <- procspec(sppspec, opt = "min")
specs.str <- procspec(sppspec, opt = c("min", "max")) # multiple options

## ---- fig=TRUE, include=TRUE, fig.cap="Results for min (left), max (centre), and both normalisations (right)", fig.show="hold", out.width="30%"----
# Plot results
plot(specs.min[, 5] ~ c(300:700), xlab = "", ylab = "", type = "l")
abline(h = 0, lty = 2)

plot(specs.max[, 5] ~ c(300:700), ylim = c(0, 1), xlab = "", ylab = "", type = "l")
abline(h = c(0, 1), lty = 2)

plot(specs.str[, 5] ~ c(300:700), type = "l", xlab = "", ylab = "")
abline(h = c(0, 1), lty = 2)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Normalised reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- results='hide'----------------------------------------------------------
# PCA analysis
spec.bin <- procspec(sppspec, opt = c("bin", "center"))
head(spec.bin)
spec.bin <- t(spec.bin) # transpose so wavelength are variables for the PCA
colnames(spec.bin) <- spec.bin[1, ]  # names variables as wavelength bins
spec.bin <- spec.bin[-1, ]  # remove 'wl' column
pca1 <- prcomp(spec.bin, scale. = TRUE)

## -----------------------------------------------------------------------------
summary(pca1)

## ---- fig=TRUE, include=TRUE, fig.cap="Plot of PC1 loading versus wavelength (left) and species mean spectra sorted vertically from lowest to highest PC1 value (right; values on right hand axis are column identities).", fig.show="hold", out.width="45%"----
# Generate colours from spectra
colr <- spec2rgb(sppspec)
wls <- as.numeric(colnames(spec.bin))

# Rank specs by PC1
sel <- rank(pca1$x[, 1])
sel <- match(names(sort(sel)), names(sppspec))

# Plot results
plot(pca1$rotation[, 1] ~ wls, type = "l", ylab = "PC1 loading")
abline(h = 0, lty = 2)
plot(sppspec, select = sel, labels.stack = names(sppspec)[sel], type = "s", col = colr)
mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)

## ---- results='hide'----------------------------------------------------------
# Create a duplicate spectrum and add some negative values
refl <- sppspec[, 7] - 20
testspecs <- as.rspec(cbind(c(300:700), refl))

# Apply two different processing options
testspecs.fix1 <- procspec(testspecs, fixneg = "addmin")
testspecs.fix2 <- procspec(testspecs, fixneg = "zero")

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=3.5, fig.cap="Plots showing original reflectance curve including negative values (left) and two processed curves using `fixneg = addmin` (center) and `fixneg = zero` (right).", fig.show="hold", out.width="30%"----
# Plot it
plot(testspecs, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

plot(testspecs.fix1, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

plot(testspecs.fix2, select = 2, ylim = c(-10, 30))
abline(h = 0, lty = 3)

mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Overlay plot of the teal angle-dependent reflectance with colours of each curve being an approximation of the perceived colour."----
data(teal)
plot(teal, type = "o", col = spec2rgb(teal))

## ---- fig=TRUE, include=TRUE, fig.cap="Stacked plot of the raw (left) and normalized (right) teal angle-dependent reflectance", fig.show="hold", out.width="45%"----
teal.norm <- procspec(teal, opt = c("min", "max"))

plot(teal, type = "s", col = spec2rgb(teal))
plot(teal.norm, type = "s", col = spec2rgb(teal))

mtext("Wavelength (nm)", side = 1, outer = T, line = 1)
mtext("Cumulative reflectance (A.U.)", side = 2, outer = T, line = 1)

## -----------------------------------------------------------------------------
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

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=4, fig.cap="Example plots created using `aggplot`. Left: using median, standard deviation, and coloured lines. Right: using mean, standard error, and greyscale", fig.show="hold", out.width="45%"----
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

## -----------------------------------------------------------------------------
butterflies <- getimg(system.file("testdata/images/", package = 'pavo'))

## -----------------------------------------------------------------------------
is.rimg(butterflies)
str(butterflies[[1]])
str(butterflies[[2]])

## -----------------------------------------------------------------------------
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


## ---- eval = FALSE------------------------------------------------------------
#  # Interactively specify the real-world scale of the image. Here 100 mm.
#  butterflies <- procimg(butterflies, scaledist = 100)

## ---- eval=FALSE--------------------------------------------------------------
#  # Interactively specify a smoothed polygon around the focal objects
#  butterflies <- procimg(butterflies, outline = TRUE, iterations = 1)

