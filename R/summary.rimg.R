#' Image summary
#'
#' Returns the attributes of, and optionally plots, an image.
#'
#' @param object (required) an image of class `rimg`, or list thereof.
#' @param plot logical; plot the image and, if the image is color-classified, the colours
#' corresponding to colour class categories side-by-side? Defaults to `FALSE`.
#' @param axes should axes be drawn when `plot = TRUE`? (defaults to `TRUE`).
#' @param col optional vector of colours when plotting colour-classified images with `plot = TRUE`.
#' Defaults to the mean RGB values of the k-means centres (i.e. the 'original' colours).
#' @param ... additional graphical options when `plot = TRUE`. Also see [par()].
#'
#' @return Either the RGB values of the k-means centres from the colour-classified image,
#' or a plot of both the image and specified colours (when `plot = TRUE`).
#'
#' @export
#'
#' @importFrom graphics image
#' @importFrom grDevices rgb
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @examples
#' \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
#' papilio_class <- classify(papilio, kcols = 4)
#' summary(papilio_class)
#'
#' # Plot the colour-classified image alongside the colour class palette
#' summary(papilio_class, plot = TRUE)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' snakes_class <- classify(snakes, kcols = 3)
#' summary(snakes_class, plot = TRUE)
#' }
#'
summary.rimg <- function(object, plot = FALSE, axes = TRUE, col = NULL, ...) {
  multi_image <- inherits(object, "list") # Single or multiple images?

  if (multi_image) {
    state <- attr(object[[1]], "state")
  } else {
    state <- attr(object, "state")
  }

  if ("colclass" %in% state) {
    if (multi_image) {
      if (plot) {
        for (i in object) {
          readline(prompt = "Press [enter] for next plot.")
          summary_main(i, plot, axes = axes, col = col, ...)
        }
      } else {
        out <- lapply(object, function(x) {
          data.frame(
            ID = attr(x, "imgname"),
            col_ID = seq(seq_len(nrow(attr(x, "classRGB")))),
            col_name = attr(x, "colnames"),
            attr(x, "classRGB"),
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, c(out, stringsAsFactors = FALSE))
      }
    } else {
      if (plot) {
        summary_main(object, plot, axes = axes, col = col, ...)
      } else {
        data.frame(
          img_ID = attr(object, "imgname"),
          col_ID = seq(seq_len(nrow(attr(object, "classRGB")))),
          col_name = attr(object, "colnames"),
          attr(object, "classRGB"),
          stringsAsFactors = FALSE
        )
      }
    }
  } else if ("raw" %in% state) {
    if (multi_image) {
      if (plot) {
        for (i in object) {
          readline(prompt = "Press [enter] for next plot.")
          plot(i, axes = axes, ...)
        }
      } else {
        out <- lapply(object, function(x) data.frame(ID = attr(x, "imgname"), stringsAsFactors = FALSE))

        do.call(rbind, c(out, stringsAsFactors = FALSE))
      }
    } else {
      if (plot) {
        plot(object, axes = axes, ...)
      } else {
        data.frame(ID = attr(object, "imgname"), stringsAsFactors = FALSE)
      }
    }
  }
}

summary_main <- function(img, plot, axes, col, ...) {
  if (plot) {

    # Plotting
    par(mfrow = c(1, 2))
    on.exit(par(mfrow = c(1, 1)))

    plot(img, axes = axes, col = col, ...)

    # Palette
    if (!is.null(col)) {
      palette <- col
    } else {
      palette <- rgb(attr(img, "classRGB"))
    }

    image(seq_along(palette), 1, as.matrix(seq_along(palette)),
      col = palette,
      xlab = paste("Colour class IDs: 1 -", length(palette)), ylab = "", xaxt = "n", yaxt = "n"
    )
  } else {
    attr(img, "classRGB")
  }
}
