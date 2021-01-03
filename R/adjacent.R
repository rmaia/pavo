#' Run an adjacency and boundary strength analysis
#'
#' Calculate summary variables from the adjacency (Endler 2012) and
#' boundary-strength (Endler et al. 2018) analyses, along with overall pattern
#' contrast (Endler & Mielke 2005).
#'
#' @param classimg (required) an xyz matrix, or list of matrices, in which x and
#'   y correspond to spatial (e.g. pixel) coordinates, and z is a numeric code
#'   specifying a colour-class. Preferably the result of [classify()], or
#'   constructed from grid-sampled spectra that have been visually modelled and
#'   clustered (as per Endler 2012).
#' @param xpts an integer specifying the number of sample points along the x
#'   axis, from which the evenly-spaced sampling grid is constructed (if
#'   required). Defaults to the smallest dimension of `classimg`, though this
#'   should be carefully considered.
#' @param xscale (required) an integer specifying the true length of the x-axis,
#'   in preferred units. Not required, and ignored, only if image scales have
#'   been set via [procimg()].
#' @param exclude the portion of the scene to be excluded from the analysis, if
#'   any.
#'   - `'none'`: default
#'   - `'background'`: exclude everything *outside* the closed polygon specified
#'      using [procimg()], or the argument `polygon`. Alternatively, if the
#'      background is relatively homogeneous the colour-class ID(s) uniquely
#'      corresponding to the background can be specified via `bkgID`, and
#'      subsequently excluded.
#'   - `'object'`: exclude everything *inside* the closed polygon specified
#'      using [procimg()], or the argument `polygon`.
#' @param bkgID an integer or vector specifying the colour-class ID number(s) of
#'   pertaining to the background alone, for relatively homogeneous and
#'   uniquely-identified backgrounds (e.g. the matte background of pinned
#'   specimens). Examine the attributes of, or call `summary` on, the result of
#'   [classify()] to visualise the RGB values corresponding to colour-class ID
#'   numbers for classified images. Ignored if the focal object and background
#'   has been identified using [procimg()].
#' @param polygon a data.frame of x-y coordinates delineating a closed polygon
#'   that separates the focal object from the background. Not required, and
#'   ignored, if the focal object outline is specified using [procimg()].
#' @param coldists a data.frame specifying the visually-modelled chromatic (dS)
#'   and/or achromatic (dL) distances between colour-categories. The first two
#'   columns should be named 'c1' and 'c2', and specify all possible
#'   combinations of numeric colour-class ID's (viewable by calling
#'   `summary(image, plot = TRUE)` on a colour classified image), with the
#'   remaining columns named dS (for chromatic distances) and/or dL (for
#'   achromatic distances). See [vismodel()] and [colspace()] for visual
#'   modelling with spectral data.
#' @param hsl data.frame specifying the hue, saturation, and luminance of color
#'   patch elements, as might be estimated via [vismodel()] and [colspace()].
#'   The first column, named 'patch', should contain numeric color category IDs,
#'   with the remaining columns specifying one or more of 'hue' (angle, in
#'   radians), 'sat', and/or 'lum'.
#' @inheritParams getspec
#'
#' @inherit getspec details
#'
#' @return a data frame of summary variables:
#' - `'k'`: The number of user-specified colour and/or luminance classes.
#' - `'N'`: The grand total (sum of diagonal and off-diagonal) transitions.
#' - `'n_off'`: The total off-diagonal transitions.
#' - `'p_i'`: The overall frequency of colour class *i*.
#' - `'q_i_j'`: The frequency of transitions between *all* colour classes *i*
#' and *j*, such that `sum(q_i_j) = 1`.
#' - `'t_i_j'`: The frequency of off-diagonal (i.e. class-change transitions)
#' transitions *i* and *j*, such that `sum(t_i_j) = 1`.
#' - `'m'`: The overall transition density (mean transitions), in units
#' specified in the argument `xscale`.
#' - `'m_r'`: The row-wise transition density (mean row transitions), in
#' user-specified units.
#' - `'m_c'`: The column-wise transition density (mean column transitions), in
#' user-specified units.
#' - `'A'`: The transition aspect ratio (< 1 = wide, > 1 = tall).
#' - `'Sc'`: Simpson colour class diversity, `Sc = 1/(sum(p_i^2))`. If all
#' colour and luminance classes are equal in relative area, then `Sc = k`.
#' - `'St'`: Simpson transition diversity, `St = 1/sum(t_i_j^2)`.
#' - `'Jc'`: Simpson colour class diversity relative to its achievable maximum.
#' `Jc = Sc/k`.
#' - `'Jt'`: Simpson transition diversity relative to its achievable maximum.
#' `Jt = St/(k*(k-1)/2)`.
#' - `'B'`: The animal/background transition ratio, or the ratio of class-change
#' transitions entirely within the focal object and those involving the object
#' and background,
#' `B = sum(O_a_a / O_a_b)`.
#' - `'Rt'`: Ratio of animal-animal and animal-background transition
#' diversities, `Rt = St_a_a / St_a_b`.
#' - `'Rab'`: Ratio of animal-animal and background-background transition
#' diversities, `Rt = St_a_a / St_b_b`.
#' - `'m_dS', 's_dS', 'cv_dS'`: weighted mean, sd, and coefficient of variation
#' of the chromatic boundary strength.
#' - `'m_dL', 's_dL', 'cv_dL'`: weighted mean, sd, and coefficient of variation
#' of the achromatic boundary strength.
#' - `'m_hue', 's_hue', 'var_hue'`: circular mean, sd, and variance of overall
#' pattern hue (in radians).
#' - `'m_sat', 's_sat', 'cv_sat'`: weighted mean, sd, and coefficient variation
#' of overall pattern saturation.
#' - `'m_lum', 's_lum', 'cv_lum'`: weighted mean, sd, and coefficient variation
#' of overall pattern luminance.
#'
#' @export
#'
#' @seealso [classify()], [summary.rimg()], [procimg()]
#'
#' @importFrom utils head object.size tail
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#'
#' @examples
#' \donttest{
#' # Set a seed, for reproducibility
#' set.seed(153)
#'
#' # Single image
#' papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
#' papilio_class <- classify(papilio, kcols = 4)
#' papilio_adj <- adjacent(papilio_class, xscale = 100)
#'
#' # Single image, with (fake) color distances and hsl values
#' # Fake color distances
#' distances <- data.frame(
#'   c1 = c(1, 1, 1, 2, 2, 3),
#'   c2 = c(2, 3, 4, 3, 4, 4),
#'   dS = c(5.3, 3.5, 5.7, 2.9, 6.1, 3.2),
#'   dL = c(5.5, 6.6, 3.3, 2.2, 4.4, 6.6)
#' )
#'
#' # Fake hue, saturation, luminance values
#' hsl_vals <- data.frame(
#'   patch = seq_len(4),
#'   hue = c(1.5, 2.2, 1.0, 0.5),
#'   lum = c(10, 5, 7, 3),
#'   sat = c(3.5, 1.1, 6.3, 1.3)
#' )
#'
#' # Full analysis, including the white background's ID
#' papilio_adj <- adjacent(papilio_class,
#'   xscale = 100, bkgID = 1,
#'   coldists = distances, hsl = hsl_vals
#' )
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' snakes_class <- classify(snakes, kcols = 3)
#' snakes_adj <- adjacent(snakes_class, xpts = 120, xscale = c(50, 55))
#' }
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#'
#' @references Endler, J. A. (2012). A framework for analysing colour pattern
#'   geometry: adjacent colours. Biological Journal Of The Linnean Society,
#'   107(2), 233-253.
#' @references Endler, J. A., Cole G., Kranz A. (2018). Boundary Strength
#'   Analysis: Combining color pattern geometry and coloured patch visual
#'   properties for use in predicting behaviour and fitness. Methods in Ecology
#'   and Evolution, 9(12), 2334-2348.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour
#'   patterns  as birds see them. Biological Journal Of The Linnean Society,
#'   86(4), 405-431.

adjacent <- function(classimg, xpts = NULL, xscale = NULL, bkgID = NULL,
                     polygon = NULL, exclude = c("none", "background", "object"),
                     coldists = NULL, hsl = NULL, cores = NULL) {
  if (!missing(cores)) {
    warning("'cores' argument is deprecated. See ?future::plan for more info ",
      "about how you can choose your parallelisation strategy.",
      call. = FALSE
    )
  }

  exclude2 <- match.arg(exclude)

  ## ------------------------------ Checks ------------------------------ ##

  ## Single or multiple images?
  multi_image <- inherits(classimg, "list")

  # Prepare if it's user-generated, and not the result of classify()
  # if(!inherits(rimg, "list")){
  #
  # }

  ## If it's a single image, store it in a list for processing convenience,
  ## before converting it back at the end
  if (!multi_image) {
    classimg <- list(classimg)
  }

  ## Class/structure
  if (!all(unlist(lapply(classimg, is.rimg)))) {
    message("One or more image matrices are not of class 'rimg'; attempting to coerce.")
    classimg <- lapply(classimg, as.rimg)
  }

  ## Colour-classified
  if (any(unlist(lapply(classimg, function(x) attr(x, "state"))) != "colclass")) {
    stop("One or more images has not yet been colour-classified. See classify().")
  }

  ## Coldists formatting
  if (!is.null(coldists)) {
    if (!inherits(coldists, "list")) {
      message("Using single set of coldists for all images.")
      coldists <- rep(list(coldists), length(classimg))
    }
    if (!all(unlist(lapply(coldists, function(x) c("c1", "c2") %in% names(x))))) {
      message("Cannot find columns named 'c1', 'c2' in coldists. Assuming first two columns contain colour-category IDs.")
      coldists <- lapply(coldists, function(x) names(x)[1:2] <- c("c1", "c2"))
    }
    if (any(unlist(lapply(coldists, function(x) !any(c("dS", "dL") %in% names(x)))))) {
      stop("One or more set of coldists without columns labelled either 'dS' or 'dL'.")
    }
  }

  ## HSL formatting
  if (!is.null(hsl)) {
    if (!inherits(hsl, "list")) {
      message("Using single set of hsl values for all images.")
      hsl <- rep(list(hsl), length(classimg))
    }
    if (!all(unlist(lapply(hsl, function(x) "patch" %in% names(x))))) {
      message("Cannot find column named 'patch' one or more set of hsl values. Assuming first column contains colour-category ID's")
      hsl <- lapply(hsl, function(x) names(x)[1] <- "patch")
    }
    if (any(unlist(lapply(hsl, function(x) !any(c("hue", "sat", "lum") %in% names(x)))))) {
      stop("One or more sets of hsl values without columns labelled either 'hue', 'sat', or 'lum'.")
    }
  }

  ## Outline formatting
  if (!is.null(polygon)) {
    if (length(polygon) != length(classimg)) {
      stop("One polygon per image is required.")
    }
    if (!all(c("x", "y") %in% names(unlist(polygon)))) {
      message("Cannot find columns named x and y in outline, taking the first two columns as x-y coordinates")
      polygon <- lapply(polygon, function(x) data.frame(x = x[, 1], y = x[, 2]))
    }
    if (all(unlist(lapply(classimg, function(x) is.na(attr(x, "outline")))))) {
      classimg <- lapply(
        seq_along(classimg),
        function(x) attr(classimg[[x]], "outline") <- polygon[[x]]
      )
    }
  }

  ## Exclusion checks
  if ("background" %in% exclude2) {
    if (is.null(bkgID) && is.null(attr(classimg, "outline"))) {
      stop("Background cannot be excluded without specifying a focal object outline (e.g. using procimg()), or one or more colour-class ID's via the argument bkgID.")
    }
  }
  if ("object" %in% exclude2) {
    if (is.null(attr(classimg, "outline"))) {
      stop("Focal object cannot be excluded without specifying its outline, (e.g. via procimg()).")
    }
  }

  ## Setting scales
  if (!is.na(attr(classimg[[1]], "px_scale"))) {
    xscale <- lapply(classimg, function(x) attr(x, "px_scale") * dim(x)[2])
  } else if (is.null(xscale)) {
    stop("Required argument xscale is missing, and one or more images are uncalibrated. Either specify xscale or use procimg() to set a scale for each image.")
  } else if (length(xscale) <= 1) {
    xscale <- as.list(rep(xscale, length(classimg)))
  } else if (length(xscale) == length(classimg)) {
    xscale <- xscale
  }

  ## Sampling density
  if (any(xpts > unlist(lapply(classimg, function(x) dim(x)[1:2])))) {
    message("Specified grid-sampling density exceeds dimensions of at least one image. Overwriting xpts to equal the smallest dimension in the image set.")
    xpts <- min(unlist(lapply(classimg, function(x) dim(x)[1:2])))
  }
  # Set to smallest dimension by default
  if (is.null(xpts)) {
    xpts <- min(unlist(lapply(classimg, function(x) dim(x)[1:2])))
  }

  xpts <- as.list(rep(xpts, length(classimg)))

  ## ------------------------------ Main ------------------------------ ##

  imgsize <- format(object.size(classimg), units = "Mb")

  with_progress({
    p <- progressor(along = classimg)
    outdata <- future_lapply(seq_along(classimg), function(x) {
      p()
      adjacent_main(
        classimg[[x]],
        xpts_i = xpts[[x]],
        xscale_i = xscale[[x]],
        bkgID_i = bkgID,
        exclude2_i = exclude2,
        coldists_i = coldists[[x]],
        hsl_i = hsl[[x]]
      )
    })
  })

  # Combine output, preserving non-shared columns. Base equivalent of; do.call(dplyr::bind_rows, outdata).
  allNms <- unique(unlist(lapply(outdata, names)))
  outdata <- do.call(rbind, c(lapply(outdata, function(x) {
    data.frame(c(x, vapply(
      setdiff(allNms, names(x)),
      function(y) NA, logical(1)
    )))
  }), make.row.names = FALSE))

  # Reshuffle column order
  namemove <- which(colnames(outdata) == "m"):which(colnames(outdata) == "cv_lum")
  outdata <- outdata[, c((seq_len(ncol(outdata)))[-namemove], namemove)]

  rownames(outdata) <- vapply(classimg, attr, "imgname", FUN.VALUE = character(1))

  # Single image
  if (length(outdata) == 1) {
    outdata <- outdata[[1]]
  }

  outdata
}


#' @importFrom stats na.omit aggregate
#' @importFrom utils head tail
adjacent_main <- function(classimg_i, xpts_i = NULL, xscale_i = NULL, bkgID_i = NULL,
                          exclude2_i = NULL, coldists_i = NULL, hsl_i = NULL) {

  ## ------------------------------ Summarising ------------------------------ ##

  c1 <- c2 <- NULL

  # Scales
  y_scale <- xscale_i / (ncol(classimg_i) / nrow(classimg_i))

  # Color names
  colournames <- attr(classimg_i, "colnames")

  # Simple or 'complex' background?
  bkgoutline <- any(!is.na(attr(classimg_i, "outline")))

  # Exclude selection, if specified
  if ("background" %in% exclude2_i) {
    # Complex backgrounds
    if (bkgoutline) {
      # NA everything *outside* the outlined polyogn
      classimg_i <- polymask(classimg_i,
        attr(classimg_i, "outline"),
        "outside",
        replacement_value = 999
      )
    } else {
      # bkgID-based version
      classimg_i[classimg_i %in% bkgID_i] <- 999
    }
  }
  if ("object" %in% exclude2_i) {
    # Complex backgrounds only
    if (bkgoutline) {
      # NA everything *inside* the outlined polyogn
      classimg_i <- polymask(classimg_i,
        attr(classimg_i, "outline"),
        "inside",
        replacement_value = 999
      )
    }
  }

  # Grid subsample
  subclass <- classimg_i[
    seq(1, nrow(classimg_i), ncol(classimg_i) / xpts_i),
    seq(1, ncol(classimg_i), ncol(classimg_i) / xpts_i)
  ]

  # Summary info
  pt_scale <- xscale_i / xpts_i # distance between grid points in user-specified units
  n_x <- ncol(subclass) # n columns
  n_y <- nrow(subclass) # n rows
  n_class <- sum(unique(c(as.matrix((subclass)))) != 999) # n color classes
  freq <- as.data.frame(table(as.matrix(subclass))) # raw class frequencies
  freq <- freq[freq$Var1 != 999, ] # remove dummy data if need be
  names(freq) <- c("patch", "Freq")
  freq$rel_freq <- freq$Freq / sum(freq$Freq) # proportion class frequency
  freq$patch <- colournames$name[as.numeric(as.character(freq$patch))]

  # Single colour check
  single_col <- n_class == 1

  # Transitions
  transitions <- transitioncalc(subclass, colournames)

  # Raw diag/offdiag
  diag <- subset(transitions[["all"]], c1 == c2)
  offdiag <- subset(transitions[["all"]], c1 != c2)

  # Proportion diag/offdiag
  diagprop <- diag
  diagprop$N <- diagprop$N / sum(diagprop$N)
  offdiagprop <- offdiag
  offdiagprop$N <- offdiagprop$N / sum(offdiagprop$N)

  ## ------------------------------ Main ------------------------------ ##

  ## ------- Adjacency (Endler 2012) ------- ##

  if (single_col) { # TODO: Fix this evil single-colour hack
    k <- n_class
    N <- sum(transitions[["all"]]$N)
    n_off <- m_r <- m_c <- m <- t <- 0
    A <- St <- Jt <- B <- Rt <- Rab <- m_dS <- s_dS <- cv_dS <- m_dL <- s_dL <- cv_dL <- NA

    p <- data.frame(t(freq$rel_freq))
    names(p) <- paste0("p_", freq$patch)

    q <- data.frame(t(transitions[["all"]]$N / sum(transitions[["all"]]$N)))
    names(q) <- paste0("q_", transitions[["all"]]$c1, "_", transitions[["all"]]$c2)

    Sc <- 1 / sum(freq$rel_freq^2)
    Jc <- Sc

    if (!is.null(hsl_i)) {
      if ("hue" %in% names(hsl_i)) m_hue <- hsl_i$hue
      if ("sat" %in% names(hsl_i)) m_sat <- hsl_i$sat
      if ("lum" %in% names(hsl_i)) m_lum <- hsl_i$lum

      s_hue <- var_hue <- NA
      s_sat <- cv_sat <- NA
      s_lum <- cv_lum <- NA
    } else {
      m_hue <- s_hue <- var_hue <- NA
      m_sat <- s_sat <- cv_sat <- NA
      m_lum <- s_lum <- cv_lum <- NA
    }
  } else {

    # n colour classes
    k <- n_class

    # Grand total transitions
    N <- sum(transitions[["all"]]$N)

    # Total off-diagonal (class-change) transitions
    n_off <- sum(offdiag$N)

    # Row transition density (mean transitions / row)
    if (nrow(subset(transitions[["row"]], c1 != c2)) == 0) {
      m_r <- 0
    } else {
      m_r <- (sum(subset(transitions[["row"]], c1 != c2)["N"]) / n_y) / xscale_i
    }

    # Col transition density (mean transitions / scaled unit)
    if (nrow(subset(transitions[["col"]], c1 != c2)) == 0) {
      m_c <- 0
    } else {
      m_c <- (sum(subset(transitions[["col"]], c1 != c2)["N"]) / n_x) / y_scale
    }

    # Transition density (mean transitions / scale)
    m <- (m_r + m_c) / 2

    # Transition aspect ratio (< 1 = wide, > 1 = tall)
    A <- m_r / m_c

    # Colour class proportions
    p <- data.frame(t(freq$rel_freq))
    names(p) <- paste0("p_", freq$patch)

    # Total transition frequencies
    q <- data.frame(t(transitions[["all"]]$N / sum(transitions[["all"]]$N)))
    names(q) <- paste0("q_", transitions[["all"]]$c1, "_", transitions[["all"]]$c2)

    # Off-diagonal transition frequencies
    t <- data.frame(t(offdiagprop$N))
    names(t) <- paste0("t_", offdiagprop$c1, "_", offdiagprop$c2)

    # Simpson diversity of colour proportions
    Sc <- 1 / sum(freq$rel_freq^2)

    # Simpson diversity of colour transitions
    St <- 1 / sum(offdiagprop$N^2)

    # Colour class diversity relative to maximum
    Jc <- Sc / k

    # Simpson transition diversity relative to maximum
    Jt <- St / (k * (k - 1) / 2)

    ## Things involving the background
    if ("none" %in% exclude2_i && any(bkgoutline, !is.null(bkgID_i))) {

      # Animal only
      if (bkgoutline) {
        anim <- polymask(classimg_i, attr(classimg_i, "outline"), "outside")
      } else if (!is.null(bkgID_i)) {
        anim <- classimg_i
        anim[anim %in% bkgID_i] <- 999
      }
      anim <- anim[
        seq(1, nrow(anim), length.out = xpts_i),
        seq(1, ncol(anim), length.out = xpts_i)
      ]
      animtrans <- transitioncalc(anim, colournames)

      # Bkg only
      if (bkgoutline) {
        bkgonly <- polymask(classimg_i, attr(classimg_i, "outline"), "outside")
      } else if (!is.null(bkgID_i)) {
        bkgonly <- classimg_i
        bkgonly[bkgonly %in% bkgID_i] <- 999
      }
      bkgonly <- bkgonly[
        seq(1, nrow(bkgonly), length.out = xpts_i),
        seq(1, ncol(bkgonly), length.out = xpts_i)
      ]
      bkgtrans <- transitioncalc(bkgonly, colournames)

      # Summary bkg metrics. Only meaningful if class-chage transitions exist
      # Check if class-change transitions actually exist
      if (nrow(subset(animtrans[["all"]], c1 != c2)) > 0 &&
        nrow(subset(bkgtrans[["all"]], c1 != c2)) > 0) {
        # Animal/background transition ratio
        B <- sum(subset(animtrans[["all"]], c1 != c2)["N"]) /
          sum(subset(transitions[["all"]], c1 != c2)["N"])

        # Animal/background transition diversity ratios Rt & Rab
        St_aa <- 1 / sum((subset(animtrans[["all"]], c1 != c2)["N"] /
          sum(subset(animtrans[["all"]], c1 != c2)["N"]))^2)
        St_bb <- 1 / sum((subset(bkgtrans[["all"]], c1 != c2)["N"] /
          sum(subset(bkgtrans[["all"]], c1 != c2)["N"]))^2)
        Rt <- St_aa / St
        Rab <- St_aa / St_bb
      } else {
        B <- Rt <- Rab <- Inf
      }
    } else {
      B <- Rt <- Rab <- NA
    }

    ## ------- Boundary strength (Endler et al. 2018) ------- ##

    if (!is.null(coldists_i)) {
      if (nrow(offdiag) > 0) { # No point if there are no class-change transitions
        # Name-match check. Could be more robust.
        if (!all(c(as.character(offdiagprop$c1), as.character(offdiagprop$c2)) %in%
          c(as.character(coldists_i$c1), as.character(coldists_i$c2)))) {
          stop("Color-classes IDs listed in coldists do not match those of the image data. Edit the IDs in coldists, or rename the color categories in the classified image data.")
        }

        offdiagprop <- merge(offdiagprop, coldists_i)

        # Chromatic calcs
        if ("dS" %in% names(offdiagprop)) {
          m_dS <- weightmean(offdiagprop$dS, offdiagprop$N)
          s_dS <- weightsd(offdiagprop$dS, offdiagprop$N)
          cv_dS <- s_dS / m_dS
        } else {
          m_dS <- s_dS <- cv_dS <- NA
        }

        # Achromatic calcs
        if ("dL" %in% names(offdiagprop)) {
          m_dL <- weightmean(offdiagprop$dL, offdiagprop$N)
          s_dL <- weightsd(offdiagprop$dL, offdiagprop$N)
          cv_dL <- s_dL / m_dL
        } else {
          m_dL <- s_dL <- cv_dL <- NA
        }
      }
    } else {
      m_dS <- s_dS <- cv_dS <- m_dL <- s_dL <- cv_dL <- NA
    }

    ## ------- Overall pattern contrasts (Endler & Mielke 2005) ------- ##

    if (!is.null(hsl_i)) {
      freq <- merge(freq, hsl_i)

      # Hue
      if ("hue" %in% names(freq)) {
        if (any(freq$hue > 2 * pi)) { # Convert to radians if need be
          message("Hue angles converted to radians")
          freq$hue <- freq$hue * (pi / 180)
        }
        m_hue <- circmean(freq$hue)
        s_hue <- circsd(freq$hue)
        var_hue <- circvar(freq$hue)
      } else {
        m_hue <- s_hue <- var_hue <- NA
      }

      # Saturation
      if ("sat" %in% names(freq)) {
        m_sat <- weightmean(freq$sat, freq$rel_freq)
        s_sat <- weightsd(freq$sat, freq$rel_freq)
        cv_sat <- s_sat / m_sat
      } else {
        m_sat <- s_sat <- cv_sat <- NA
      }

      # Luminance
      if ("lum" %in% names(freq)) {
        m_lum <- weightmean(freq$lum, freq$rel_freq)
        s_lum <- weightsd(freq$lum, freq$rel_freq)
        cv_lum <- s_lum / m_lum
      } else {
        m_lum <- s_lum <- cv_lum <- NA
      }
    } else {
      m_hue <- s_hue <- var_hue <- NA
      m_sat <- s_sat <- cv_sat <- NA
      m_lum <- s_lum <- cv_lum <- NA
    }
  }

  # Output
  fin <- data.frame(
    k, N, n_off, p, q, t, m, m_r, m_c, A, Sc, St, Jc, Jt, B, Rt,
    Rab, m_dS, s_dS, cv_dS, m_dL, s_dL, cv_dL, m_hue, s_hue, var_hue,
    m_sat, s_sat, cv_sat, m_lum, s_lum, cv_lum
  )

  fin <- as.data.frame(fin)

  fin
}


# Internal function for masking color-classified image data that fall inside/outside a polygon
#' @importFrom sp point.in.polygon
polymask <- function(imagedat,
                     polygon,
                     alter_which = c("outside", "inside"),
                     replacement_value = NA) {
  imglong <- data.frame(expand.grid(seq_len(ncol(imagedat)), seq_len(nrow(imagedat))), z = c(imagedat))
  names(imglong) <- c("x", "y", "z")

  inpoly <- point.in.polygon(imglong$x, imglong$y, polygon$x, polygon$y, mode.checked = FALSE) # todo: replace with base

  maskmat <- matrix(data = inpoly, ncol(imagedat), nrow(imagedat))
  maskmat <- apply(as.matrix(maskmat), 1, rev)
  maskmat <- rev(t(apply(as.matrix(maskmat), 1, rev))) # mirror (DOUBLECHECK)
  if (alter_which == "inside") {
    imagedat[which(maskmat == 1)] <- replacement_value
    imagedat[which(maskmat == 2)] <- replacement_value
    imagedat[which(maskmat == 3)] <- replacement_value
  }
  if (alter_which == "outside") {
    imagedat[which(maskmat == 0)] <- replacement_value
  }
  imagedat
}

## Transition calculator
transitioncalc <- function(classimgdat, colornames) {
  transout <- list()

  # All row transitions
  rt_temp <- lapply(
    seq_len(nrow(classimgdat)),
    function(x) {
      table(paste0(
        head(as.numeric(classimgdat[x, ]), -1),
        ".", tail(as.numeric(classimgdat[x, ]), -1)
      ))
    }
  )
  rt <- aggregate(unlist(rt_temp) ~ names(unlist(rt_temp)), FUN = sum)
  rt <- rt[!grepl("NA", rt[, 1]), ] # Remove columns containing NA's (i.e. bkg, if chosen to exclude)
  rnames <- as.numeric(unlist(strsplit(rt[, 1], "[.]"))) # split up transition names
  rowtrans <- data.frame(
    "c1" = rnames[seq(1, length(rnames), 2)],
    "c2" = rnames[seq(2, length(rnames), 2)],
    "N" = rt[, 2]
  )

  # All column transitions
  classimgdat_trans <- as.data.frame(t(classimgdat))
  ct_temp <- lapply(
    seq_len(nrow(classimgdat_trans)),
    function(x) {
      table(paste0(
        head(as.numeric(classimgdat_trans[x, ]), -1),
        ".", tail(as.numeric(classimgdat_trans[x, ]), -1)
      ))
    }
  )
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

  # Rename
  for (i in unique(c(transitions$c1, transitions$c2))) {
    transitions$c1[transitions$c1 == i] <- colornames[i, ]
    transitions$c2[transitions$c2 == i] <- colornames[i, ]
  }
  for (i in unique(c(coltrans2$c1, coltrans2$c2))) {
    coltrans2$c1[coltrans2$c1 == i] <- colornames[i, ]
    coltrans2$c2[coltrans2$c2 == i] <- colornames[i, ]
  }
  for (i in unique(c(rowtrans2$c1, rowtrans2$c2))) {
    rowtrans2$c1[rowtrans2$c1 == i] <- colornames[i, ]
    rowtrans2$c2[rowtrans2$c2 == i] <- colornames[i, ]
  }

  # Ditch any excluded colours (999's, via NA's in name)
  coltrans2 <- na.omit(coltrans2)
  if (nrow(coltrans2) == 0) {
    coltrans2[1, ] <- NA
  }
  rowtrans2 <- na.omit(rowtrans2)
  if (nrow(rowtrans2) == 0) {
    rowtrans2[1, ] <- NA
  }
  transitions <- na.omit(transitions)

  transout[["col"]] <- coltrans2
  transout[["row"]] <- rowtrans2
  transout[["all"]] <- transitions

  transout
}
