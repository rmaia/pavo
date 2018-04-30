#' Colour-classified image summary
#'
#' Returns the attributes of the colour-classified matrix generated from \code{\link{classify}}.
#'
#' @param object (required) Results of \code{\link{classify}}.
#' @param plot logical; plot the actual colours corresponding to colour class
#' categories? Defaults to \code{FALSE}.
#' @param ... class consistency (ignored)
#'
#' @return Either the RGB values of the k-means centres from the colour-classified image,
#' or a plot of the colours specified by those values (when \code{plot = TRUE}.
#'
#' @export
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, n_cols = 4)
#' summary(papilio_class)
#'
#' # Plot the unprocessed and colour-classified images alongside the colour class samples
#' par(mfrow = c(1,3))
#' plot(papilio)
#' plot(papilio_class)
#' summary(papilio_class, plot = TRUE)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

summary.rimg <- function(object, plot = FALSE, ...) {
  values <- attr(object, "classRGB")

  if (isTRUE(plot)) {
    graphics::image(1:nrow(values), 1, as.matrix(1:nrow(values)),
      col = grDevices::rgb(values),
      xlab = paste("Colour class IDs:", paste(rownames(values), collapse = ", ")), ylab = "", xaxt = "n", yaxt = "n"
    )
  } else {
    attr(object, "classRGB")
  }
}
