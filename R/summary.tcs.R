#' Tetracolorspace avian visual model summary
#'
#' Calculates characteristics of a cloud of points (spectra) in avian tetrahedral color space
#'
#' @export summary.tcs
#' @method summary tcs
#' @import geometry
#' @param object (required) Results of \code{tcs} or Can be either the result
#' from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#' with four columns, representing the avian cones).
#' @param by either a single value specifying the range of color points for which
#' summary colorspace variables should be calculated (for example, \code{by} = 3 
#' indicates summary will be calculated for groups of 3 consecutive color points (rows)
#' in the quantum catch color data frame) or a vector containing identifications for 
#' the rows in the quantum catch color data frame (in which case summaries will be 
#' calculated for each group of points sharing the same identification). If \code{by} 
#' is left blank, the summary statistics are calculated accross all color points in the
#' data. 
#' @param ... class consistency (ignored)
#' 
#' @return a \code{data.frame} containing summary statistics with groups of points in rows and 
#' the following variables in columns:
#' @return \code{centroid.u, .s, .m, .l} the centroids of \code{usml} coordinates of points
#' @return \code{c.vol} the total volume occupied by the points
#' @return \code{colspan.m} the mean hue span
#' @return \code{colspan.v} the variance in hue span
#' @return \code{mean.ra} mean saturation
#' @return \code{max.ra} maximum saturation achieved by the group of points
#'
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis)
#' summary(tcs.sicalis, by=rep(c('C','T','B'),7))}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

summary.tcs <- function(object, by=NULL, ...){

if(!is.null(by)){
    
    if(length(by)==1){
    by.many <- by
    by <- rep(1:(dim(object)[1]/by),each=by)
    by <- factor(by,labels=row.names(object)[seq(1,length(row.names(object)),by=by.many)])
    }

  by <- factor(by)
  res.c <- data.frame(t(sapply(levels(by),function(z)tcssum(object[which(by==z),]))))
  row.names(res.c) <- levels(by)
    
  }else{
    res.c <- data.frame(t(tcssum(object)))
    row.names(res.c) <- 'all.points'
    }

if(NA %in% res.c$cvol)
  warning('Not enough points to calculate volume', call.=FALSE)

res.c
}
