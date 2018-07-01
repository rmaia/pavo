#' Colorspace data summary
#'
#' Returns the attributes of \code{colspace} objects.
#'
#' @import geometry
#'
#' @param object (required) a \code{colspace} object.
#' @param by when the input is in \code{tcs} colorspace, \code{by} is either
#'  a single value specifying the range of color points for which
#'  summary tetrahedral-colorspace variables should be calculated (for example, \code{by} = 3
#'  indicates summary will be calculated for groups of 3 consecutive color points (rows)
#'  in the quantum catch color data frame) or a vector containing identifications for
#'  the rows in the quantum catch color data frame (in which case summaries will be
#'  calculated for each group of points sharing the same identification). If \code{by}
#'  is left blank, the summary statistics are calculated across all color points in the
#'  data.
#' @param ... class consistency (ignored).
#'
#' @return returns all attributes of the data as mapped to the selected colourspace, including
#' options specified when calculating the visual model. Also return the default
#' \code{data.frame} summary, except when the object is the result of \code{tcs},
#' in which case the following variables are output instead:
#' @return \code{centroid.u, .s, .m, .l} the centroids of \code{usml} coordinates of points.
#' @return \code{c.vol} the total volume occupied by the points.
#' @return \code{rel.c.vol} volume occupied by the points relative to the tetrahedron volume.
#' @return \code{colspan.m} the mean hue span.
#' @return \code{colspan.v} the variance in hue span.
#' @return \code{huedisp.m} the mean hue disparity.
#' @return \code{huedisp.v} the variance in hue disparity.
#' @return \code{mean.ra} mean saturation.
#' @return \code{max.ra} maximum saturation achieved by the group of points.
#'
#' @export
#'
#' @examples \dontrun{
#' # Colour hexagon
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE,
#'                         vonkries = TRUE, bkg = 'green')
#' flowers.hex <- hexagon(vis.flowers)
#' summary(flowers.hex)
#'
#' # Tetrahedral model
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' csp.sicalis <- colspace(vis.sicalis)
#' summary(csp.sicalis, by = rep(c('C', 'T', 'B'), 7))}
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings.
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

summary.colspace <- function(object, by = NULL, ...){

  if(is.null(attr(object, 'clrsp'))){
    message('Cannot return full colspace summary on subset data')
    return(summary(as.data.frame(object)))
  }

  cat("Colorspace & visual model options:\n",
      '* Colorspace:', attr(object, 'clrsp'), '\n',
      '* Quantal catch:', attr(object, 'qcatch'), '\n',
      '* Visual system, chromatic:', attr(object,'visualsystem.chromatic'), '\n',
      '* Visual system, achromatic:', attr(object,'visualsystem.achromatic'), '\n',
      '* Illuminant:', attr(object,'illuminant'), '\n',
      '* Background:', attr(object,'background'), '\n',
      '* Relative:', attr(object, 'relative'), '\n', '\n'
  )

  if(attr(object, 'clrsp') != 'tcs') summary.data.frame(object)

  if(attr(object, 'clrsp') == 'tcs'){

    if(!is.null(by)){

        if(length(by) == 1){
          by.many <- by
          by <- rep(1:(dim(object)[1]/by), each = by)
          by <- factor(by,
            labels = row.names(object)[seq(1, length(row.names(object)), by = by.many)])
          }

      by <- factor(by)
      res.c <- data.frame(t(sapply(levels(by),function(z) tcssum(object[which(by == z),]))))
      row.names(res.c) <- levels(by)

      }else{
        res.c <- data.frame(t(tcssum(object)))
        row.names(res.c) <- 'all.points'
        }

    if(is.na(res.c$cvol))
      warning('Not enough points to calculate volume', call. = FALSE)

    res.c
  }
}
