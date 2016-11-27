## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ------------------------------------------------------------------------
data(flowers)

head(flowers[1:4])

## ----fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_The visual sensitivities of the muscoid fly Musca domestica._"----

plot(as.rspec(vissyst[, c('wl', 'musca.u', 'musca.s', 'musca.m', 'musca.l', 'md.r1')]), main = 'Musca domestica', ylab = 'Absorbance')


## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Flowers in a dichromatic colorspace, as modelled according to a canid visual system._"----
plot(di.flowers, pch = 21, bg = 'forestgreen') 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'fi', scale = 10000)

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Floral reflectance in a Maxwell triangle, considering a honeybee visual system._"----
plot(tri.flowers, pch = 21, bg = 'forestgreen') 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'bluetit', qcatch = 'fi', scale = 10000)

tetra.flowers <- colspace(vis.flowers, space = 'tcs')

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_Flowers in a tetrahedral colorspace, with varied orientations, modelled using the visual phenotype of the blue tit._"----
par(mfrow = c(1, 2))
plot(tetra.flowers, view = 100, pch = 21, bg = 'forestgreen') 
plot(tetra.flowers, view = 75, pch = 21, bg = 'forestgreen')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')

## ------------------------------------------------------------------------
hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Flowers as modelled in the hymenopteran colour hexagon of Chittka (1992), overlain with coarse bee-hue sectors._"----
plot(hex.flowers, sectors = 'coarse', pch = 21, bg = 'forestgreen')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')

coc.flowers <- colspace(vis.flowers, space = 'coc')

head(coc.flowers)


## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Flowers in the color-opponent-coding space of Backhaus (1991), as modelling according to the honeybee._"----
plot(coc.flowers, pch = 21, bg = 'forestgreen') 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE, achromatic = 'none')

## ------------------------------------------------------------------------
ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
head(ciexyz.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Floral reflectance in the CIEXYZ human visual model. Note that this space is not perceptually calibrated, so we cannot make inferences about the similarity or differences of colours based on their relative location._"----
plot(ciexyz.flowers, pch = 21, bg = 'forestgreen') 

## ------------------------------------------------------------------------

cielab.flowers <- colspace(vis.flowers, space = 'cielab')
head(cielab.flowers)


## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_CIELAB._"----
plot(cielab.flowers, pch = 21, bg = 'forestgreen') 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achro = 'none', relative = TRUE)

## ------------------------------------------------------------------------
cat.flowers <- colspace(vis.flowers, space = 'categorical')

head(cat.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Flowers in the categorical colorspace of Troje (1993)._"----
plot(cat.flowers, pch = 21, bg = 'forestgreen') 

