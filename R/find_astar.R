#' Compute the \eqn{\alpha^*}{alpha*} value
#'
#' @param coords coordinates of the points for which you want to compute the
#'   alphashape
#'
#' @return The \eqn{\alpha^*}{alpha*} value
#'  as defined in Gruson (2020).
#'
#' @seealso [alphashape3d::ashape3d()]
#'
#' @keywords internal
#'
#' @references
#' Gruson H. 2020. Estimation of colour volumes as concave hypervolumes using
#'  \eqn{\alpha}{alpha}-shapes. Methods in Ecology and Evolution, early view
#'  \doi{10.1111/2041-210X.13398}
find_astar <- function(coords) {
  tetras <- alphashape3d::ashape3d(coords,
    alpha = 0
  )$tetra
  tetras <- cbind(
    c(tetras[, c("v1", "v2", "v3", "v4")]),
    rep(tetras[, "rhoT"], 4)
  )
  astar <- max(by(tetras[, 2], tetras[, 1], min))

  message("'avalue' automatically set to ", sprintf("%.4e", astar * (1 + 1e-15)))

  # We have to round the value because of numerical precision issues
  return(astar * (1 + 1e-15))
}
