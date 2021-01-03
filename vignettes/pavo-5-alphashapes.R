## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.show = "hold",
  eval = identical(Sys.getenv("IN_PKGDOWN"), "true")
)

## ----include = FALSE, eval=require("rgl")-------------------------------------
knitr::knit_hooks$set(webgl = rgl::hook_webgl)

## ---- include=FALSE-----------------------------------------------------------
#  knitr::opts_knit$set(global.device = TRUE)

## -----------------------------------------------------------------------------
#  library(pavo)
#  data(flowers)
#  vis_flowers <- vismodel(flowers, visual = "avg.uv")
#  tcs_flowers <- colspace(vis_flowers)
#  plot(tcs_flowers)

## -----------------------------------------------------------------------------
#  vol(tcs_flowers, type = "alpha")

## ---- include=FALSE-----------------------------------------------------------
#  knitr::opts_knit$set(global.device = FALSE)

## -----------------------------------------------------------------------------
#  summary(tcs_flowers)

## -----------------------------------------------------------------------------
#  plot(tcs_flowers)
#  vol(tcs_flowers, type = "alpha", avalue = 0.5)

## -----------------------------------------------------------------------------
#  cd_flowers <- coldist(vis_flowers)
#  xy_flowers <- jnd2xyz(cd_flowers)
#  plot(xy_flowers)

## -----------------------------------------------------------------------------
#  library(alphashape3d)
#  ashape_jnd <- ashape3d(as.matrix(xy_flowers), alpha = 10)
#  volume_ashape3d(ashape_jnd)

## ---- echo = FALSE------------------------------------------------------------
#  rgl::bg3d("white")

## ---- webgl=TRUE--------------------------------------------------------------
#  plot(ashape_jnd)

## -----------------------------------------------------------------------------
#  data(sicalis)
#  tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
#  tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")

## ---- echo = FALSE------------------------------------------------------------
#  rgl::bg3d()

## ---- webgl=TRUE--------------------------------------------------------------
#  voloverlap(tcs.sicalis.C, tcs.sicalis.B, type = "alpha", plot = TRUE)

