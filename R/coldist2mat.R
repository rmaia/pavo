#' Convert coldist to distance matrix
#'
#' Converts a \code{coldist} output into a distance matrix where samples
#' are rows and columns.
#'
#' @param coldistres (required) the output from a \code{coldist} call.
#'
#' @return A list containing one or two matrices, for dS and dL, depending
#' if the original object had dS and dL columns
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, achro = TRUE)
#' cd.flowers <- coldist(vis.flowers)
#' coldist2mat(cd.flowers)[["dS"]]
#' coldist2mat(cd.flowers)[["dL"]]
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal
#'
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#'  Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}

coldist2mat <- function(coldistres) {
  cdrep <- as.matrix(rbind(coldistres[, c(1, 2, 3)], coldistres[, c(2, 1, 3)]))

  uniquepatches <- unique(c(coldistres[, 1], coldistres[, 2]))

  dSM <- matrix(nrow = length(uniquepatches), ncol = length(uniquepatches))

  rownames(dSM) <- uniquepatches
  colnames(dSM) <- uniquepatches

  dSM[cdrep[, 1:2] ] <- cdrep[, 3]
  dSM[cdrep[, 2:1] ] <- cdrep[, 3]

  class(dSM) <- "numeric"
  # dSM[is.na(dSM)] <- 0
  diag(dSM) <- 0

  res <- list(dS = dSM)

  if ("dL" %in% colnames(coldistres)) {
    cdrepL <- as.matrix(rbind(coldistres[, c(1, 2, 4)], coldistres[, c(2, 1, 4)]))

    dLM <- matrix(nrow = length(uniquepatches), ncol = length(uniquepatches))
    rownames(dLM) <- uniquepatches
    colnames(dLM) <- uniquepatches

    dLM[cdrepL[, 1:2] ] <- cdrepL[, 3]
    dLM[cdrepL[, 2:1] ] <- cdrepL[, 3]

    class(dLM) <- "numeric"
    dLM[is.na(dLM)] <- 0

    res$dL <- dLM
  }

  res
}
