#' Run an adjacency analysis
#'
#' Calculate summary variables from the adjacency analysis (Endler 2012 & Endler et al. 2018).
#'
#' @param classimg (required) an xyz image matrix, or list of matrices, in which
#' x and y correspond to pixel coordinates, and z is a numeric code specifying
#' a colour-class. Preferably the result of \code{\link{classify}}.
#' @param x_pts (required) an integer specifying the number of sample points, or grid-sampling
#' density, along the x axis. Y-axis sampling density is calculated automatically
#' from this, to maintain an even grid spacing.
#' @param x_scale (required) an integer specifying the true length of the x-axis,
#' in preferred units. Not required, and ignored, if image scales have been set via
#' \code{\link{calibrate}}.
#' @param bkg_ID an integer specifying the colour-class ID number of the homogeneous background.
#' Examine the attributes of, or call \code{summary} on, the result of \code{\link{classify}}
#' to visualise the RGB values corresponding to colour-class ID numbers.
#' @param bkg_include logical; should the background be excluded from the analyses?
#' Defaults to \code{FALSE}.
#'
#' @return a data frame of summary variables:
#' \itemize{
#'   \item \code{'k'}: The number of user-specified colour and/or luminance classes.
#'   \item \code{'N'}: The grand total (sum of diagonal and off-diagonal) transitions.
#'   \item \code{'n_off'}: The total off-diagonal transitions.
#'   \item \code{'Obs_i_j'}: The observed number of off-diagonal (class-change)
#'     transitions, such that \code{sum(Obs_i_j) = n_off}.
#'   \item \code{'E_i_j'}: The expected number of off-diagonal (class-change)
#'     transitions, where \code{E_i_j = 2 x n_off x p_i x p_j}.
#'   \item \code{'d_t_o'}: The absolute deviation from expected off-diagonal (class-change)
#'     transitions, calculated on the observed transition matrix, where
#'     d_t_o = sum(abs(O_i_j - E_i_j)).
#'   \item \code{'p_i'}: The overall frequency of colour class \emph{i}.
#'   \item \code{'q_i_j'}: The frequency of transitions between \emph{all} colour classes
#'    \emph{i} and \emph{j}, such that \code{sum(q_i_j) = 1}.
#'   \item \code{'t_i_j'}: The frequency of off-diagonal (i.e. class-change
#'     transitions) transitions \emph{i} and \emph{j}, such that \code{sum(t_i_j) = 1}.
#'   \item \code{'m'}: The overall transition density (mean transitions),
#'     in units specified in the argument \code{x_scale}.
#'   \item \code{'m_r'}: The row transition density (mean row transitions),
#'     in user-specified units.
#'   \item \code{'m_c'}: The column transition density (mean column transitions),
#'     in user-specified units.
#'   \item \code{'A'}: The transition aspect ratio (< 1 = wide, > 1 = tall).
#'   \item \code{'Sc'}: Simpson colour class diversity.
#'   \item \code{'St'}: Simpson transition diversity.
#'   \item \code{'Jc'}: Simpson colour class diversity relative to its achievable maximum.
#'   \item \code{'Jt'}: Simpson transition diversity relative to its achievable maximum.
#'   }
#'
#' @export
#' 
#' @importFrom dplyr bind_rows
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, n_cols = 4)
#' papilio_adj <- adjacent(papilio_class, x_pts = 150, x_scale = 100)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes_class <- classify(snakes, n_cols = 3)
#' snakes_adj <- adjacent(snakes_class, x_pts = 250, x_scale = 50)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @references Endler, J. A. (2012). A framework for analysing colour pattern geometry:
#' adjacent colours. Biological Journal Of The Linnean Society, 86(4), 405-431.

adjacent <- function(classimg, x_pts = NULL, x_scale = NULL, bkg_ID = NULL, bkg_include = TRUE) {

  ## Checks
  multi_image <- inherits(classimg, "list") # Single or multiple images?

  ## Setting scales
  # Single image
  if (!isTRUE(multi_image)) {
    if (!is.null(attr(classimg, "px_scale"))) {
      x_scale <- attr(classimg, "px_scale") * dim(classimg)[2]
    } else if (is.null(attr(classimg, "px_scale")) && is.null(x_scale)) {
      stop("Required argument x_scale is missing, and image data are uncalibrated. Either
         specify x_scale or use calibrate() to set a scale.")
    }
    ## Multi images
  } else if (isTRUE(multi_image)) {
    if (!is.null(attr(classimg[[1]], "px_scale"))) {
      x_scale <- lapply(1:length(classimg), function(x) attr(classimg[[x]], "px_scale") * dim(classimg[[x]])[2])
    } else if (is.null(attr(classimg[[1]], "px_scale")) && !is.null(x_scale)) {
      x_scale <- lapply(1:length(classimg), function(x) x_scale)
    } else if (is.null(attr(classimg[[1]], "px_scale")) && is.null(x_scale)) {
      stop("Required argument x_scale is missing, and one or more images are uncalibrated.
           Either specify x_scale or use calibrate() to set a scale for each image.")
    }
  }

  if (isTRUE(multi_image)) { # Multiple images
    outdata <- lapply(1:length(classimg), function(x) adjacent_main(classimg[[x]],
        x_pts_i = x_pts,
        x_scale_i = x_scale[[x]],
        bkg_ID_i = bkg_ID,
        bkg_include_i = bkg_include
      ))
    outdata <- do.call(bind_rows, outdata)
    for (i in 1:nrow(outdata)) {
      rownames(outdata)[i] <- attr(classimg[[i]], "imgname")
    }
  } else if (!isTRUE(multi_image)) { # Single image

    outdata <- adjacent_main(
      classimg_i = classimg,
      x_pts_i = x_pts,
      x_scale_i = x_scale,
      bkg_ID_i = bkg_ID,
      bkg_include_i = bkg_include
    )
    rownames(outdata) <- attr(classimg, "imgname")
  }
  outdata
}

#' Main function for adjacency analysis
#'
#' @param classimg_i (required) an xyz image matrix, or list of matrices, in which
#' x and y correspond to pixel coordinates, and z is a numeric code specifying
#' a colour-class. Preferably the result of \code{\link{classify}}.
#' @param x_pts_i (required) an integer specifying the number of sample points, or grid-sampling
#' density, along the x axis. Y-axis sampling density is calculated automatically
#' from this, to maintain an even grid spacing.
#' @param x_scale_i (required) an integer specifying the true length of the x-axis,
#' in preferred units. Not required, and ignored, if image scales have been set via
#' \code{\link{calibrate}}.
#' @param bkg_ID_i an integer specifying the colour-class ID number of the homogeneous background.
#' Examine the attributes of, or call \code{summary} on, the result of \code{\link{classify}}
#' to visualise the RGB values corresponding to colour-class ID numbers.
#' @param bkg_include_i logical; should the background be excluded from the analyses?
#' Defaults to \code{FALSE}.
#'
#' @keywords internal
#' 
#' @importFrom stats na.omit aggregate
#' @importFrom utils head tail
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @return a data frame of summary variables. See \code{\link{adjacent}} for details.
#'
adjacent_main <- function(classimg_i, x_pts_i = NULL, x_scale_i = 1, bkg_ID_i = NULL, bkg_include_i = TRUE) {
  c1 <- c2 <- NULL

  # Integrity check
  if (bkg_include_i == FALSE && is.null(bkg_ID_i)) {
    stop("Background cannot be excluded without specifying its ID via the argument bkg_ID.")
  }

  # Scales
  y_scale <- x_scale_i / (ncol(classimg_i) / nrow(classimg_i))

  # Subsample, if specified
  if (length(x_pts_i) == 1) {
    subclass <- classimg_i[
      seq(1, nrow(classimg_i), ncol(classimg_i) / x_pts_i),
      seq(1, ncol(classimg_i), ncol(classimg_i) / x_pts_i)
    ]
  } else if (is.null(x_pts_i)) {
    x_pts_i <- c(dim(classimg_i))
  }

  # Exclude background, if specified
  if (!is.null(bkg_ID_i) && bkg_include_i == FALSE) {

    # Render background NA
    subclass[subclass == bkg_ID_i] <- NA

    # Subset matrix to include only rows with at least one non-bkg transition
    subclass <- subclass[rowSums(!is.na(subclass)) > 1, colSums(!is.na(subclass)) > 1]
  }

  # Summary info
  pt_scale <- x_scale_i / x_pts_i # distance between grid sample points in user-specified units
  n_x <- ncol(subclass) # n columns
  n_y <- nrow(subclass) # n rows
  # n_area <- sum(rowSums(!is.na(subclass))) # total number of non-NA pixels
  n_class <- length(na.omit(unique(c(as.matrix((subclass)))))) # n color classes
  freq <- as.data.frame(table(as.matrix(subclass))) # raw class frequencies
  freq$rel_freq <- freq$Freq / sum(freq$Freq) # proportion class frequency

  # All row transitions
  rt_temp <- lapply(1:nrow(subclass), function(x) table(paste0(head(as.numeric(subclass[x, ]), -1), ".", tail(as.numeric(subclass[x, ]), -1))))
  rt <- aggregate(unlist(rt_temp) ~ names(unlist(rt_temp)), FUN = sum)
  rt <- rt[!grepl("NA", rt[, 1]), ] # Remove columns containing NA's (i.e. bkg, if chosen to exclude)
  rnames <- as.numeric(unlist(strsplit(rt[, 1], "[.]"))) # split up transition names
  rowtrans <- data.frame(
    "c1" = rnames[seq(1, length(rnames), 2)],
    "c2" = rnames[seq(2, length(rnames), 2)],
    "N" = rt[, 2]
  )

  # All column transitions
  subclass_trans <- as.data.frame(t(subclass))
  ct_temp <- lapply(1:nrow(subclass_trans), function(x) table(paste0(head(as.numeric(subclass_trans[x, ]), -1), ".", tail(as.numeric(subclass_trans[x, ]), -1))))
  ct <- aggregate(unlist(ct_temp) ~ names(unlist(ct_temp)), FUN = sum)
  ct <- ct[!grepl("NA", ct[, 1]), ] # Remove columns containing NA's (i.e. bkg, if chosen to exclude)
  cnames <- as.numeric(unlist(strsplit(ct[, 1], "[.]"))) # split up transition names
  coltrans <- data.frame(
    "c1" = cnames[seq(1, length(cnames), 2)],
    "c2" = cnames[seq(2, length(cnames), 2)],
    "N" = ct[, 2]
  )

  # Sort
  rowtrans[, 1:2] <- as.data.frame(t(apply(rowtrans[, 1:2], 1, sort)))
  coltrans[, 1:2] <- as.data.frame(t(apply(coltrans[, 1:2], 1, sort)))

  # Aggregate
  rowtrans2 <- aggregate(rowtrans$N ~ rowtrans$c1 + rowtrans$c2, FUN = sum)
  names(rowtrans2) <- c("c1", "c2", "N")
  coltrans2 <- aggregate(coltrans$N ~ coltrans$c1 + coltrans$c2, FUN = sum)
  names(coltrans2) <- c("c1", "c2", "N")

  # All raw transitions
  transitions <- rbind(rowtrans2, coltrans2)
  transitions <- aggregate(transitions$N ~ transitions$c1 + transitions$c2, FUN = sum)
  names(transitions) <- c("c1", "c2", "N")

  # Raw diag/offdiag
  diag <- subset(transitions, c1 == c2)
  offdiag <- subset(transitions, c1 != c2)

  # Proportion diag/offdiag
  diagprop <- diag
  diagprop$N <- diagprop$N / sum(diagprop$N)
  offdiagprop <- offdiag
  offdiagprop$N <- offdiagprop$N / sum(offdiagprop$N)

  ## Summary variables  ##

  # Internal
  ## -----------------------------##




  # Output
  ## -----------------------------##

  # n colour classes
  k <- n_class

  # Grand total transitions
  N <- sum(transitions$N)

  # Total off-diagonal (class-change) transitions
  n_off <- sum(offdiag$N)

  # Row transition density (mean transitions / row)
  if (nrow(subset(rowtrans2, c1 != c2)) == 0) {
    m_r <- 0.001
  } else {
    m_r <- (sum(subset(rowtrans2, c1 != c2)["N"]) / n_y) / x_scale_i
  }

  # Col transition density (mean transitions / scaled unit)
  if (nrow(subset(coltrans2, c1 != c2)) == 0) {
    m_c <- 0.001
  } else {
    m_c <- (sum(subset(coltrans2, c1 != c2)["N"]) / n_x) / y_scale
  }

  # Transition density (mean transitions / scale)
  m <- (m_r + m_c) / 2

  # Transition aspect ratio (< 1 = wide, > 1 = tall)
  A <- m_r / m_c

  # Colour class proportions
  p <- data.frame(t(freq$rel_freq))
  names(p) <- paste0("p_", freq$Var1)

  # Total transition frequencies
  q <- data.frame(t(transitions$N / sum(transitions$N)))
  names(q) <- paste0("q_", transitions$c1, "_", transitions$c2)

  # Observed off-diagonal transitions
  Obs <- data.frame(t(offdiag$N))
  names(Obs) <- paste0("Obs_", offdiag$c1, "_", offdiag$c2)

  # Expected frequency of off-diagonal transitions
  offdiag$exp <- NA
  for (i in 1:nrow(offdiag)) {
    offdiag$exp[i] <- 2 * n_off * freq$rel_freq[offdiag[i, 1]] * freq$rel_freq[offdiag[i, 2]]
  }
  E <- data.frame(t(offdiag$exp))
  names(E) <- paste0("E_", offdiag$c1, "_", offdiag$c2)

  # Deviations
  d_t_o <- sum(abs(offdiag$N - offdiag$exp))

  # Off-diagonal transition frequencies
  t <- data.frame(t(offdiagprop$N))
  names(t) <- paste0("t_", offdiagprop$c1, "_", offdiagprop$c2)

  # Simpson diversity of colour proportions
  Sc <- 1 / sum(freq$rel_freq^2)

  # Simpson diversity of colour transitions
  St <- 1 / sum(q^2)

  # Colour class diversity relative to maximum
  Jc <- Sc / k

  # Simpson transition diversity relative to maximum
  Jt <- St / (k * (k - 1) / 2)

  # Output
  fin <- data.frame(k, N, n_off, Obs, E, d_t_o, p, q, t, m, m_r, m_c, A, Sc, St, Jc, Jt)

  fin <- as.data.frame(fin)

  fin
}
