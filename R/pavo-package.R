#' @keywords internal
"_PACKAGE"

#' @importFrom grDevices colorRampPalette rgb
#' @importFrom graphics abline axis image legend lines mtext par plot points polygon title arrows
#' @importFrom stats approx cor dist loess.smooth median quantile sd var
#' @importFrom utils combn head tail
NULL

.PlotTetraEnv <- new.env()
.PlotCielabEnv <- new.env()
.PlotJND2XYZEnv <- new.env()
