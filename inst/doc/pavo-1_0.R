## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ----fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_The flower dataset_"----
data(flowers)

head(flowers[1:4])
plot(flowers, lwd=2, col=spec2rgb(flowers))

## ----fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_The visual sensitivities of the muscoid fly Musca domestica._"----

plot(vissyst[, c('wl', grep('musca|md',names(vissyst), value=TRUE))], main = 'Musca domestica', ylab = 'Absorbance', lwd=2)

## ----fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_Transmission example: Ocular transmission for the blue tit (red) and blackbird (blue) retinas._"----

plot(transmissiondata, lwd=2, ylab='Transmission', main = 'Ocular transmission', col=c('red','blue'))

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

## ---- fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_Flowers in a tetrahedral colorspace, with varied orientations and perspectives, modelled using the visual phenotype of the blue tit._"----
par(mfrow = c(1, 2))
plot(tetra.flowers, view = 30, pch = 21, bg = 'forestgreen') 
plot(tetra.flowers, view=60, scale.y=0.6, pch = 21, bg = 'forestgreen')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')

## ------------------------------------------------------------------------
hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Flowers as modelled in the hymenopteran color hexagon of Chittka (1992), overlain with coarse bee-hue sectors._"----
plot(hex.flowers, sectors = 'coarse', pch = 21, bg = 'forestgreen')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, bkg = 'green')

coc.flowers <- colspace(vis.flowers, space = 'coc')

head(coc.flowers)


## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Flowers in the color-opponent-coding space of Backhaus (1991), as modelling according to the honeybee._"----
plot(coc.flowers, pch = 21, bg = 'forestgreen', yaxt='n') 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE, achromatic = 'none')

## ------------------------------------------------------------------------
ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
head(ciexyz.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Floral reflectance in the CIEXYZ human visual model. Note that this space is not perceptually calibrated, so we cannot make inferences about the similarity or differences of colors based on their relative location._"----
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

## ----eval=FALSE----------------------------------------------------------
#  data(sicalis)
#  vis.sicalis <- vismodel(sicalis, relative=FALSE)
#  JND.sicalis <- coldist(vis.sicalis, n1=1, n2=2, n3=2, n4=4, v=0.2)

## ------------------------------------------------------------------------
data(sicalis)
vis.sicalis <- vismodel(sicalis, relative=FALSE)
JND.sicalis <- coldist(vis.sicalis, n=c(1,2,2,4), weber=0.1, weber.ref=4)
head(JND.sicalis)

## ------------------------------------------------------------------------
data(sicalis)
vis.sicalis <- vismodel(sicalis, relative=FALSE)
JND.sicalis <- coldist(vis.sicalis, n=c(1,2,2,4), weber=0.1, weber.ref='longest')
head(JND.sicalis)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Visual system of a pretend mantis shrimp with 10 cones_"----
# create an arbitrary visual phenotype with 10 photoreceptors
fakemantisshrimp <- sensmodel(c(325,350,400,425,450,500,550,600,650,700), beta=FALSE, integrate=FALSE)

# convert to percentages, just to color the plot 
fakemantisshrimp.colors <- fakemantisshrimp*100
fakemantisshrimp.colors[,'wl'] <- fakemantisshrimp[,'wl']
 
plot(fakemantisshrimp, col=spec2rgb(fakemantisshrimp.colors), lwd=2, ylab='Absorbance')

# run visual model and calculate color distances

vm.fms <- vismodel(flowers, visual=fakemantisshrimp, relative=FALSE, achro=FALSE)

JND.fms <- coldist(vm.fms, n=c(1,1,2,2,3,3,4,4,5,5))

head(JND.fms)

