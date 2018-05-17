#' @keywords internal
"_PACKAGE"

#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom graphics abline axis image legend lines mtext par plot points polygon title arrows
#' @importFrom stats approx cor dist loess.smooth median quantile runif sd var
#' @importFrom utils combn head read.table setTxtProgressBar tail txtProgressBar
# #' @importFrom rgl open3d spheres3d
NULL

.PlotTetraEnv <- new.env()
.PlotCielabEnv <- new.env()
.PlotJND2XYZEnv <- new.env()
