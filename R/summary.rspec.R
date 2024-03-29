#' Colourimetric variables
#'
#' Calculates all 23 colourimetric variables reviewed in Montgomerie (2006).
#'
#' @param object (required) a data frame, possibly an object of class `rspec`,
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to process.
#' @param subset Either `FALSE` (the default), `TRUE`, or a character vector.
#' If `FALSE`, all variables calculated are returned. If `TRUE`, only a subset
#' of the complete output (composed of B2, S8 and H1; the variables described in
#' Andersson and Prager 2006) are returned. Finally, a user-specified string of variable
#' names can be used in order to filter and show only those variables.
#' @param lim The range of wavelengths used in calculations. The default is to use
#' the entire range in the `rspec` object (typically equivalent to `lim = c(300, 700)`).
#' @param wlmin,wlmax Deprecated. Use the `lim` argument instead.
#' @param ... class consistency (ignored)
#'
#' @return A data frame containing either 23 or 5 (`subset = TRUE`) variables described
#' in Montgomerie (2006) with spectra name as row names.
#' The colorimetric variables calculated by this function are
#' described in Montgomerie (2006) with corrections included in the README CLR
#' file from the May 2008 distribution of the CLR software. Authors should reference
#' both this package,Montgomerie (2006), and the original reference(s).
#' Description and notes on the measures:
#'
#' B1 (Total brightness): Sum of the relative reflectance over the entire spectral
#' range (area under the curve). Frequently used but should be discouraged because
#' values are difficult to compare across studies (B2 is preferred). REF 1-3, 7, 9-11,
#' 13
#'
#' B2 (Mean brightness): Mean relative reflectance over the entire spectral range.
#' This is preferred to B1 since values are easier to compare across studies. REF 4, 12
#'
#' B3 (Intensity): Maximum relative reflectance (Reflectance at wavelength of maximum
#' reflectance). Note that may be sensitive to noise near the peak. REF 1, 5, 6
#'
#' S1 (Chroma): Relative contribution of a spectral range to the total brightness (B1)
#' S1 is arbitrarily divided in 6 measures of chroma based on the wavelength ranges
#' normally associated with specific hues. The values are calculated using the
#' following ranges: S1U (UV, if applicable): lambda min-400nm;
#' S1V (Violet) lambda min-415nm; S1B (Blue) 400nm-510nm; S1G (Green) 510nm-605nm;
#' S1Y (Yellow) 550nm-625nm; S1R (Red) 605nm-lambda max. REF 2, 7, 8, 11-13
#'
#' S2 (Spectral saturation): Rmax/Rmin This measure is sensitive to spectral noise.
#' Proper interpretation of this value may be difficult for spectra with multiple
#' peaks in the range of interest. REF 1
#'
#' S3 (Chroma): Reflectance over the Rmax +- 50nm range divided by B1. Values for peaks
#' within 50nm of either the minimum or maximum range of the data will not be comparable
#' since the area under the curve for the area of interest will not always
#' be based on the same wavelength range. Therefore, S3 should be interpreted
#' with caution for peaks in the UV or Red range. REF 11
#'
#' S4 (Spectral purity): |bmaxneg| , calculated by approximating the derivative
#' of the spectral curve. As such, it is very sensitive to noise and should only
#' be considered when data is adequately smoothed. NAs are returned for curves which
#' do not, at any range of wavelength, decrease in intensity. Therefore, reflectance
#' curves for brown and red surfaces, for example, should not generate a values. REF 1
#'
#' S5 (Chroma): Similar in design to segment classification measures (see Montgomerie 2006
#' for details). REF 10
#'
#' S6 (Contrast): Rmax - Rmin. Because it uses both Rmin and Rmax, this measure may be
#' sensitive to spectral noise. REF 5, 6
#'
#' S7 (Spectral saturation): Difference between the relative reflectance before and after
#' the wavelength at which reflectance is halfway between its minimum (Rmin)
#' and its maximum (Rmax). Somewhat sensitive
#' to noise and can be misleading when more than one maxima and/or minima are present.
#' REF 3, 9
#'
#' S8 (Chroma): (Rmax - Rmin)/B2. Because it uses both Rmin and Rmax, this measure may be
#' sensitive to spectral noise. REF 3, 13
#'
#' S9 (Carotenoid chroma): (R700 - R450)/R700. Should only be used when the colour
#' of the surface is clearly due to carotenoid pigmentation and R450 is lower than
#' R700. Could be sensitive to noise. REF 8
#'
#' S10 (Peaky chroma): (Rmax - Rmin)/B2 x |bmaxneg|. Should be used with properly
#' smoothed curves. REF 7
#'
#' H1 (Peak wavelength, hue): Wavelength of maximum reflectance. May be sensitive to noise
#' and may be variable if there is more than one maxima. REF 1, 2, 4, 6, 7, 10-13
#'
#' H2 (Hue): Wavelength at bmaxneg. Should be calculated using smoothed data. REF 2, 13
#'
#' H3 (Hue): Wavelength at Rmid. Sensitive to noisy spectra and may be variable if there are
#' more than one maxima and minima. REF 3, 9, 13
#'
#' H4 (Hue): Similar in design to segment classification measures see Montgomerie
#' (2006) for details. REF 10
#'
#' H5 (Hue): Wavelength at bmax. Sensitive to noise and may be variable if there is
#' more than one maxima and minima. REF 5
#'
#' @note If minimum wavelength is over 400, UV chroma is not computed.
#' @note Variables which compute bmax and bmaxneg should be used with caution, for they
#' rely on smoothed curves to remove noise, which would otherwise result in spurious
#' results. Make sure chosen smoothing parameters are adequate.
#' @note Smoothing affects only B3, S2, S4, S6, S10, H2, and H5 calculation. All other
#' variables can be reliably extracted using non-smoothed data.
#'
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' # Load data
#' data(sicalis)
#'
#' # Calculate and display all spectral summary variables
#' summary(sicalis)
#'
#' # Calculate only subset of B2, S8 and H1 as per Andersson (1999)
#' summary(sicalis, subset = TRUE)
#'
#' # Calculate user-specified subset of B1 and H4
#' summary(sicalis, subset = c("B1", "H4"))
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' @author Pierre-Paul Bitton \email{bittonp@@windsor.ca}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @references Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J., eds.
#' Bird Coloration. Volume 1 Mechanisms and measurements. Harvard University Press, Cambridge, Massachusetts.
#' @references References describing variables:
#'
#' 1- Andersson, S. 1999. Morphology of uv reflectance in a whistling-thrush: Implications for the study
#' of structural colour signalling in birds. Journal of Avian Biology 30:193-204.
#'
#' 2- Andersson, S., J. Ornborg, and M. Andersson. 1998. Ultraviolet sexual dimorphism and assortative
#' mating in blue tits. Proceedings of the Royal Society B 265:445-450.
#'
#' 3- Andersson, S., S. Pryke, J. Ornborg, M. Lawes, and M. Andersson. 2002. Multiple receivers, multiple
#' ornaments, and a trade-off between agonistic and epigamic signaling in a widowbird. American
#' Naturalist 160:683-691.
#'
#' 4- Delhey, K., A. Johnsen, A. Peters, S. Andersson, and B. Kempenaers. 2003. Paternity analysis reveals
#' opposing selection pressures on crown coloration in the blue tit (Parus caeruleus). Proceedings
#' of the Royal Society B 270:2057-2063.
#'
#' 5- Keyser, A. and G. Hill. 1999. Condition-dependent variation in the blue-ultraviolet coloration of a
#' structurally based plumage ornament. Proceedings of the Royal Society B 266:771-777.
#'
#' 6- Keyser, A.J. and G. Hill. 2000. Structurally based plumage coloration is an honest signal of quality in
#' male blue grosbeaks. Behavioural Ecology 11:202-209.
#'
#' 7- Ornborg, J., S. Andersson, S. Griffith, and B. Sheldon. 2002. Seasonal changes in a ultraviolet
#' structural colour signal in blue tits, Parus caeruleus. Biological Journal of the Linnean Society
#' 76:237-245.
#'
#' 8- Peters, A., A. Denk, K. Delhey, and B. Kempenaers. 2004. Carotenoid-based bill colour as an
#' indicator of immunocompetence and sperm performance in male mallards. Journal of
#' Evolutionary Biology 17:1111-1120.
#'
#' 9- Pryke, S., M. Lawes, and S. Andersson. 2001. Agonistic carotenoid signalling in male red-collared
#' widowbirds: Aggression related to the colour signal of both the territory owner and model
#' intruder. Animal Behaviour 62:695-704.
#'
#' 10- Saks, L., K. Mcgraw, and P. Horak. 2003. How feather colour reflects its carotenoid content.
#' Functional Ecology 17:555-561.
#'
#' 11- Shawkey, M., A. Estes, L. Siefferman, and G. Hill. 2003. Nanostructure predicts intraspecific
#' variation in ultraviolet-blue plumage colour. Proceedings of the Royal Society B
#' 270:1455-1460.
#'
#' 12- Siefferman, L. and G. Hill. 2005. UV-blue structural coloration and competition for nestboxes in male
#' eastern bluebirds. Animal Behaviour 69:67-72.
#'
#' 13- Smiseth, P., J. Ornborg, S. Andersson, and T. Amundsen. 2001. Is male plumage reflectance
#' correlated with paternal care in bluethroats? Behavioural Ecology 12:164-170.
#'
summary.rspec <- function(object, subset = FALSE, lim = NULL, wlmin = NULL, wlmax = NULL, ...) {
  chkDots(...)

  wl <- isolate_wl(object, keep = "wl")

  # The control-flow here is for cases when users specify only of the deprecated
  # wlmin or wlmax arguments. Can be removed once deprecated in next release.
  if (!all(missing("wlmin"), missing("wlmin"))) {
    warning(
      "The 'wlmin' and 'wlmax' arguments are deprecated as of v2.9.0,",
      "and will be removed in future releases. Use the lim() argument instead.",
      call. = FALSE
    )
    if (!missing("wlmin") && missing("wlmax")) {
      lim <- c(wlmin, max(wl))
    }
    if (missing("wlmin") && !missing("wlmax")) {
      lim <- c(min(wl), wlmax)
    }
    if (all(missing("wlmin"), missing("wlmin"))) {
      lim <- c(wlmin, wlmax)
    }
  }

  lambdamin <- max(lim[1], min(wl))
  lambdamax <- min(lim[2], max(wl))

  # wl-range checks
  if (!missing(lim)) {
    if (lambdamin > lim[1]) {
      stop("Minimum specified wavelength is smaller than the range of spectral data. Check the lim argument.", call. = FALSE)
    }
    if (lambdamax < lim[2]) {
      stop("Maximum specified wavelength is larger than the range of spectral data. Check the lim argument.", call. = FALSE)
    }
  }

  # Restrict to range of wlmin:wlmax
  object <- object[which(wl == lambdamin):which(wl == lambdamax), ]

  wl <- isolate_wl(object, keep = "wl")
  object <- isolate_wl(object, keep = "spec")

  # Establish variables and handle any subsetting
  variable_names <- c(
    "B1", "B2", "B3", "S1", "S2", "S3", "S4", "S5", "S6",
    "S7", "S8", "S9", "S10", "H1", "H2", "H3", "H4", "H5"
  )

  if (is.logical(subset)) {
    if (subset) {
      variable_names <- c("B2", "S8", "H1")
    }
  } else {
    if (!all(subset %in% variable_names)) {
      stop("Names in ", dQuote("subset"), " do not match color variable names", call. = FALSE)
    }
    variable_names <- subset
  }

  # Preserve original variable names
  variable_names_orig <- variable_names

  # Replace S1 with sub-variables
  if ("S1" %in% variable_names) {
    # Find index of 'S1'
    s1_index <- which(variable_names == "S1")

    # Remove 'S1'
    variable_names <- variable_names[-s1_index]

    # Full names
    new_var_names <- c("S1U", "S1V", "S1B", "S1G", "S1Y", "S1R")

    # Append full names at the right place
    variable_names <- append(variable_names, new_var_names, after = s1_index - 1)
  }

  # Define final summary output df
  color.var <- data.frame(matrix(
    ncol = length(variable_names),
    nrow = ncol(object)
  ))
  names(color.var) <- variable_names

  # Dictionary to store computed variables
  computed_vars <- list()

  # Define a dependency structure among variables.
  # This allows us to speed up computation by computing only the necessary
  # variables (and their dependencies) when subset is not FALSE.
  dependencies <- list(
    B1 = list(fun = calc_B1, deps = NULL),
    B2 = list(fun = calc_B2, deps = NULL),
    B3 = list(fun = calc_B3, deps = NULL),
    S1 = list(fun = calc_S1, deps = "B1"),
    S2 = list(fun = calc_S2, deps = c("B3", "Rmin")),
    S3 = list(fun = calc_S3, deps = c("H1", "B1")),
    S4 = list(fun = calc_S4, deps = NULL),
    S5 = list(fun = calc_S5, deps = NULL),
    S6 = list(fun = calc_S6, deps = c("B3", "Rmin")),
    S7 = list(fun = calc_S7, deps = c("B1", "Rmid")),
    S8 = list(fun = calc_S8, deps = c("S6", "B2")),
    S9 = list(fun = calc_S9, deps = NULL),
    S10 = list(fun = calc_S10, deps = c("S8", "S4")),
    H1 = list(fun = calc_H1, deps = NULL),
    H2 = list(fun = calc_H2, deps = NULL),
    H3 = list(fun = calc_H3, deps = c("B3", "Rmin", "Rmid")),
    H4 = list(fun = calc_H4, deps = NULL),
    H5 = list(fun = calc_H5, deps = NULL),
    Rmin = list(fun = calc_Rmin, deps = NULL),
    Rmid = list(fun = calc_Rmid, deps = NULL)
  )

  # Function to calculate a variable, which ensures each variable
  # is calculated only once
  compute_color_var <- function(var_name) {
    # If the variable is already computed, return it
    if (!is.null(computed_vars[[var_name]])) {
      return(computed_vars[[var_name]])
    }

    # Get the function and its dependencies
    fun <- dependencies[[var_name]]$fun
    deps <- dependencies[[var_name]]$deps

    # If the function has dependencies, calculate them first
    if (!is.null(deps)) {
      for (dep in deps) {
        computed_vars[[dep]] <- compute_color_var(dep)
      }
    }

    # Calculate and store variable
    computed_vars[[var_name]] <- fun(object, wl, computed_vars)

    # Special case handling for S1, which returns a list
    if (var_name == "S1") {
      for (sub_var_name in names(computed_vars[[var_name]])) {
        color.var[, sub_var_name] <- computed_vars[[var_name]][[sub_var_name]]
      }
    } else {
      color.var[, var_name] <- computed_vars[[var_name]]
    }

    # Return the computed variable
    return(computed_vars[[var_name]])
  }

  # Calculate all specified variables
  for (var_name in variable_names_orig) {
    if (var_name == "S1") {
      color.var[, c("S1U", "S1V", "S1B", "S1G", "S1Y", "S1R")] <- compute_color_var("S1")
    } else {
      color.var[, var_name] <- compute_color_var(var_name)
    }
  }

  row.names(color.var) <- names(object)

  if ("Rmid" %in% names(color.var)) {
    color.var <- color.var[, names(color.var) != "Rmid"]
  }
  if ("Rmin" %in% names(color.var)) {
    color.var <- color.var[, names(color.var) != "Rmin"]
  }

  # The double-conversion here is just so the attributes are in the same order
  # as in the original formulation, for CI test consistency
  as.data.frame(as.matrix(color.var))
}


## ----- Common intermediaries ----- ##

calc_Rmin <- function(object, wl, lambdamin, lambdamax) {
  vapply(object, min, numeric(1))
}

calc_Rmid <- function(object, wl, lambdamin, lambdamax) {
  (calc_B3(object) + calc_Rmin(object)) / 2
}

## ----- Brightness ----- ##

calc_B1 <- function(object, wl, computed_vars, ...) {
  colSums(object)
}

calc_B2 <- function(object, wl, computed_vars, ...) {
  colMeans(object)
}

calc_B3 <- function(object, wl, computed_vars, ...) {
  vapply(object, max, numeric(1))
}

## ----- Saturation ----- ##

calc_S1 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  B1 <- computed_vars[["B1"]]
  lambdamin <- min(wl)
  lambdamax <- max(wl)

  # Define the chroma ranges
  chroma_ranges <- list(
    S1U = list(
      min = lambdamin, max = 400,
      message = "cannot calculate UV chroma; wavelength range not below 400 nm"
    ),
    S1V = list(
      min = lambdamin, max = 415,
      message = "cannot calculate violet chroma; wavelength below 415 nm"
    ),
    S1B = list(
      min = 400, max = 510,
      message = "cannot calculate blue chroma; wavelength range not between 400 and 510 nm"
    ),
    S1G = list(
      min = 510, max = 605,
      message = "cannot calculate green chroma; wavelength range not between 510 and 605 nm"
    ),
    S1Y = list(
      min = 550, max = 625,
      message = "cannot calculate yellow chroma; wavelength range not between 550 and 625 nm"
    ),
    S1R = list(
      min = 605, max = 700,
      message = "cannot calculate red chroma; wavelength range not between 605 and 700 nm"
    )
  )

  output.mat <- matrix(ncol = length(chroma_ranges), nrow = ncol(object))

  for (i in seq_along(chroma_ranges)) {
    if (lambdamin <= chroma_ranges[[i]]$min && lambdamax >= chroma_ranges[[i]]$max) {
      chromamat <- object[wl >= chroma_ranges[[i]]$min & wl <= chroma_ranges[[i]]$max, , drop = FALSE]
      chroma <- colSums(chromamat) / B1
      output.mat[, i] <- chroma
    } else {
      warning(chroma_ranges[[i]]$message, call. = FALSE)
    }
  }

  if (lambdamin > 300 && lambdamin < 400) {
    warning("Minimum wavelength is ", lambdamin, "; UV-related variables may not be meaningful", call. = FALSE)
  }

  return(
    list(
      S1U = output.mat[, 1],
      S1V = output.mat[, 2],
      S1B = output.mat[, 3],
      S1G = output.mat[, 4],
      S1Y = output.mat[, 5],
      S1R = output.mat[, 6]
    )
  )
}

calc_S2 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  B3 <- computed_vars[["B3"]]
  Rmin <- computed_vars[["Rmin"]]

  # Calc
  B3 / Rmin
}

calc_S3 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  H1 <- computed_vars[["H1"]]
  B1 <- computed_vars[["B1"]]

  # Calc
  S3 <- vapply(seq_len(ncol(object)), function(col) {
    spec <- object[, col]
    H1_spec <- H1[col]
    sum(spec[wl >= (H1_spec - 50) & wl <= (H1_spec + 50)])
  }, numeric(1))
  S3 / B1
}

calc_S4 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  diffsmooth <- apply(object, 2, diff)
  incr <- apply(diffsmooth, 2, min) > 0

  # Calc
  bmaxneg <- abs(apply(diffsmooth, 2, min))
  bmaxneg[incr] <- NA
  bmaxneg
}

calc_S5 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  lambdamin <- min(wl)
  lambdamax <- max(wl)

  # Calc
  segmts <- trunc(quantile(lambdamin:lambdamax, names = FALSE))
  Q1 <- wl >= segmts[1] & wl <= segmts[2]
  Q2 <- wl >= segmts[2] & wl <= segmts[3]
  Q3 <- wl >= segmts[3] & wl <= segmts[4]
  Q4 <- wl >= segmts[4] & wl <= segmts[5]
  S5R <- colSums(object[Q4, , drop = FALSE])
  S5Y <- colSums(object[Q3, , drop = FALSE])
  S5G <- colSums(object[Q2, , drop = FALSE])
  S5B <- colSums(object[Q1, , drop = FALSE])
  sqrt((S5R - S5G)^2 + (S5Y - S5B)^2)
}

calc_S6 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  B3 <- computed_vars[["B3"]]
  Rmin <- computed_vars[["Rmin"]]

  # Calc
  B3 - Rmin
}

calc_S7 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  B1 <- computed_vars[["B1"]]
  Rmid <- computed_vars[["Rmid"]]

  # Calc
  index_Rmid <- vapply(seq_len(ncol(object)), function(x) {
    which.min(abs(object[, x] - Rmid[x]))
  }, numeric(1))
  S7 <- vapply(seq_len(ncol(object)), function(col) {
    spec <- object[, col]
    index_Rmid_spec <- index_Rmid[col]
    spec_low <- spec[seq_len(index_Rmid_spec)]
    spec_high <- spec[index_Rmid_spec:length(spec)]
    return(sum(spec_low) - sum(spec_high))
  }, numeric(1))
  S7 / B1
}

calc_S8 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  S6 <- computed_vars[["S6"]]
  B2 <- computed_vars[["B2"]]

  # Calc
  S6 / B2
}

calc_S9 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  R450 <- as.numeric(object[which(wl == 450), , drop = FALSE])
  R700 <- as.numeric(object[which(wl == 700), , drop = FALSE])

  # Calc
  (R700 - R450) / R700
}

calc_S10 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  S8 <- computed_vars[["S8"]]
  S4 <- computed_vars[["S4"]]

  # Calc
  S8 * S4
}

## ----- Hue ----- ##

calc_H1 <- function(object, wl, computed_vars, ...) {
  # Calc
  wl[max.col(t(object), ties.method = "first")]
}

calc_H2 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  diffsmooth <- apply(object, 2, diff)
  incr <- apply(diffsmooth, 2, min) > 0
  decr <- apply(diffsmooth, 2, max) < 0

  # Calc
  lambdabmaxneg <- wl[apply(diffsmooth, 2, which.min)]
  lambdabmaxneg[incr] <- NA
  lambdabmaxneg
}

calc_H3 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  B3 <- computed_vars[["B3"]]
  Rmin <- computed_vars[["Rmin"]]
  Rmid <- computed_vars[["Rmid"]]
  index_Rmid <- vapply(seq_len(ncol(object)), function(x) {
    which.min(abs(object[, x] - Rmid[x]))
  }, numeric(1))

  # Calc
  wl[index_Rmid]
}

calc_H4 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  lambdamin <- min(wl)
  lambdamax <- max(wl)
  segmts <- trunc(quantile(lambdamin:lambdamax, names = FALSE))
  Q1 <- wl >= segmts[1] & wl <= segmts[2]
  Q2 <- wl >= segmts[2] & wl <= segmts[3]
  Q3 <- wl >= segmts[3] & wl <= segmts[4]
  Q4 <- wl >= segmts[4] & wl <= segmts[5]
  S5R <- colSums(object[Q4, , drop = FALSE])
  S5Y <- colSums(object[Q3, , drop = FALSE])
  S5G <- colSums(object[Q2, , drop = FALSE])
  S5B <- colSums(object[Q1, , drop = FALSE])

  # Calc
  atan2(S5Y - S5B, S5R - S5G)
}

calc_H5 <- function(object, wl, computed_vars, ...) {
  # Dependencies
  diffsmooth <- apply(object, 2, diff)
  decr <- apply(diffsmooth, 2, max) < 0

  # Calc
  lambdabmax <- wl[apply(diffsmooth, 2, which.max)]
  lambdabmax[decr] <- NA
  lambdabmax
}
