## ---- echo=FALSE, warning=FALSE, results='hide', message=FALSE-----------
library(pavo)

## ------------------------------------------------------------------------
data(flowers)

head(flowers[1:4])

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'canis')

di.flowers <- colspace(vis.flowers, space = 'di')

head(di.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Dichromatic space._"----
plot(di.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'apis')

tri.flowers <- colspace(vis.flowers, space = 'tri')

head(tri.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=6, fig.height=6, fig.align='center', fig.cap="_Maxwell triangle._"----
plot(tri.flowers) 

## ------------------------------------------------------------------------
vis.flowers <- vismodel(flowers, visual = 'bluetit')

tetra.flowers <- colspace(vis.flowers, space = 'tcs')

head(tetra.flowers)

## ---- fig=TRUE, include=TRUE, fig.width=7.2, fig.height=5, fig.align='center', fig.cap="_Tetrahedral colorspace._"----
par(mfrow = c(1, 2))
plot(tetra.flowers, view = 105) 
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

