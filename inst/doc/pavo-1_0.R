## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ------------------------------------------------------------------------
data(flowers)

head(flowers[1:4])

## ----fig=TRUE, include=TRUE, warnings=FALSE, message=FALSE, echo=FALSE, fig.width=8, fig.height=28, fig.align='center', fig.cap="_The visual phenotypes included in pavo 1.0._"----
par(mfrow = c(10, 2))
plot(as.rspec(cbind(vissyst$wl, vissyst[2:5])), main = 'Average avian UV', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[14:17])), main = 'Average avian V', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[6:9], vissyst$bt.dc)), main = 'Blue tit', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[10:13], vissyst$st.dc)), main = 'Starling', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[18:21], vissyst$ch.dc)), main = 'Peafowl', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[25:27])), main = 'Honeybee', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[28:29])), main = 'Canis familiaris', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[30:32])), main = 'CIE 2-degree', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[33:35])), main = 'CIE 10-degree', ylab = 'Absorbance')
plot(as.rspec(cbind(vissyst$wl, vissyst[36:39], vissyst$md.r1)), main = 'Musca domestica', ylab = 'Absorbance')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.align='center', fig.cap="_Flowers in a dichromatic colorspace, as modelled according to a canid visual system._"----
plot(di.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'fi', scale = 10000)

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Floral reflectance in a Maxwell triangle, considering a honeybee visual system._"----
plot(tri.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'bluetit', qcatch = 'fi', scale = 10000)

tetra.flowers <- colspace(vis.flowers, space = 'tcs')

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_Flowers in a tetrahedral colorspace, with varied orientations, modelled using the visual phenotype of the blue tit._"----
par(mfrow = c(1, 2))
plot(tetra.flowers, view = 100) 
plot(tetra.flowers, view = 75)

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')

hex.flowers <- colspace(vis.flowers, space = 'hexagon')

head(hex.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Colour hexagon._"----
plot(hex.flowers, sectors = 'coarse')

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE)

coc.flowers <- colspace(vis.flowers, space = 'coc')

head(coc.flowers)


## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_coc._"----
plot(coc.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'cie2', illum = 'D65')

ciexyz.flowers <- colspace(vis.flowers, space = 'ciexyz')
cielab.flowers <- colspace(vis.flowers, space = 'cielab')

head(ciexyz.flowers)

head(cielab.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_CIEXYZ._"----
plot(ciexyz.flowers) 

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_CIELAB._"----
plot(cielab.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achro = 'none', relative = TRUE)

cat.flowers <- colspace(vis.flowers, space = 'categorical')

head(cat.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Cat._"----
plot(cat.flowers) 

