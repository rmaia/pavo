#' Run an adjacency analysis
#'
#' Calculate summary variables from the adjacency analysis (Endler 2012 & Endler et al. 2018).
#'
#' @param classimg (required) an xyz image matrix, or list of matrices, in which
#' x and y correspond to pixel coordinates, and z is a numeric code specifying
#' a colour-class. Preferably the result of \code{\link{classify}}.
#' @param xscale (required) an integer specifying the true length of the x-axis,
#' in preferred units. Not required, and ignored, if image scales have been set via
#' \code{\link{procimg}}.
#' @param xpts an integer specifying the number of sample points, or grid-sampling
#' density, along the x axis. Defaults to the size of the x-dimension (i.e. every pixel).
#' Y-axis sampling density is calculated automatically from this, to maintain an even
#' grid spacing.
#' @param exclude The portion of the image to be excluded from the analysis, if any.
#' If excluding the focal object, its outline must first have been idenfitied using 
#' \code{\link{procimg}}. If excluding the image background it must either have been
#' identified using \code{\link{procspec}}, or if it is relatively homogeneous, 
#' the colour-class ID's corresponding to the background can be specified using 
#' bkgID.
#' @param bkgID an integer or vector specifying the colour-class ID number(s) of
#' pertaining to the background. Examine the attributes of, or call \code{summary} on,
#' the result of \code{\link{classify}} to visualise the RGB values corresponding to
#' colour-class ID numbers. Ignored if the focal object and background has been identified using
#' \code{\link{procimg}}. 
#' @param coldists A data.frame specifying the visually-modelled chromatic (dS)
#' and/or achromatic (dL) distances between colour-categories. The first two columns
#' should specify all possible combinations of colour category ID's, and be named 'c1'
#' and 'c2', with the remaining columns named dS (for chromatic distances) and/or dL
#' (for achromatic distances). See \code{\link{vismodel}} and \code{\link{colspace}}
#' for visual modelling with spectral data.
#' @param cores number of cores to be used in parallel processing. If \code{1}, parallel
#'  computing will not be used. Defaults to \code{getOption("mc.cores", 2L)}.
#'
#' @return a data frame of summary variables. Only variables with at least
#' one non-NA value are returned:
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
#'     in units specified in the argument \code{xscale}.
#'   \item \code{'m_r'}: The row transition density (mean row transitions),
#'     in user-specified units.
#'   \item \code{'m_c'}: The column transition density (mean column transitions),
#'     in user-specified units.
#'   \item \code{'A'}: The transition aspect ratio (< 1 = wide, > 1 = tall).
#'   \item \code{'B'}: The animal/background transition ratio.
#'   \item \code{'Sc'}: Simpson colour class diversity.
#'   \item \code{'St'}: Simpson transition diversity.
#'   \item \code{'Jc'}: Simpson colour class diversity relative to its achievable maximum.
#'   \item \code{'Jt'}: Simpson transition diversity relative to its achievable maximum.
#'   \item \code{'Rt'}: Ratio of animal-animal and animal-background transition diversities,
#'   \code{Rt = St_a_a / St_a_b}.
#'   \item \code{'Rab'}: Ratio of animal-animal and background-background transition diversities,
#'   \code{Rt = St_a_a / St_b_b}.
#'   \item \code{'m_dS'}: weighted mean of the chromatic boundary strength.
#'   \item \code{'s_dS'}: weighted standard deviation of the chromatic boundary strength.
#'   \item \code{'cv_dS'}: weighted coefficient of variation of the chromatic boundary strength.
#'   \item \code{'m_dL'}: weighted mean of the achromatic boundary strength.
#'   \item \code{'s_dL'}: weighted standard deviation of the achromatic boundary strength.
#'   \item \code{'cv_dL'}: weighted coefficient of variation of the achromatic boundary strength.
#'   }
#'
#' @export
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom utils object.size
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, kcols = 4)
#' papilio_adj <- adjacent(papilio_class, xpts = 150, xscale = 100)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes_class <- classify(snakes, kcols = 3)
#' snakes_adj2 <- adjacent(snakes_class, xpts = 250, xscale = 50)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @references Endler, J. A. (2012). A framework for analysing colour pattern geometry:
#' adjacent colours. Biological Journal Of The Linnean Society, 86(4), 405-431.
#' @references Endler, J. A., Cole G., Kranz A.  (2018). Boundary Strength Analysis:
#' Combining color pattern geometry and coloured patch visual properties for use in predicting behaviour
#' and fitness. Methods in Ecology and Evolution.

adjacent <- function(classimg, xscale = NULL, xpts = NULL, bkgID = NULL,
                     exclude = c('none', 'background', 'object'), coldists = NULL, 
                     cores = getOption("mc.cores", 2L)) {

  exclude2 <- match.arg(exclude)

  ## Checks
  # Single or multiple images?
  multi_image <- inherits(classimg, "list")

  # Coldists formatting (individual/multiple? todo)
  if (!is.null(coldists)) {
    if (ncol(coldists) < 3) {
      stop("Too few columns present in 'coldists' data.frame.")
    }
    if (!all(c("c1", "c2") %in% names(coldists))) {
      warning("Cannot find columns named 'c1', 'c2' in coldists. Assuming first two columns
              contain colour-category IDs.")
      names(coldists)[1:2] <- c("c1", "c2")
    }
    if (!any(c("dS", "dL") %in% names(coldists))) {
      stop("No columns named 'dS' and/or 'dL' in coldists.")
    }
    # Need to sort?
  }

  # Exclusion checks
  if ('background' %in% exclude2) {
    if (is.null(bkgID) && is.null(attr(classimg, 'outline'))){
      stop("Background cannot be excluded without specifying a focal object outline 
            (e.g. using procimg()), or one or more colour-class ID's via the argument bkgID.")
    }
  }
  if ('object' %in% exclude2) {
    if (is.null(attr(classimg, 'outline'))){
      stop("Focal object cannot be excluded without specifying its outline, (e.g. via procimg()).")
    }
  }
  
  # if (multi_image) {
  #   n_class <- length(na.omit(unique(c(as.matrix((classimg[[1]]))))))
  # } else {
  #   n_class <- length(na.omit(unique(c(as.matrix((classimg))))))
  # }
  # if(bkg.include == FALSE && length(bkgID) - n_class < 1)                        #FIX
  #   stop('No colour classes remaining.')

  ## Setting scales
  # Single image
  if (!multi_image) {
    if (!is.null(attr(classimg, "px_scale"))) {
      xscale <- attr(classimg, "px_scale") * dim(classimg)[2]
    } else if (is.null(attr(classimg, "px_scale")) && is.null(xscale)) {
      stop("Required argument xscale is missing, and image data are uncalibrated. Either
         specify xscale or use procimg() to set a scale.")
    }
    ## Multi images
  } else if (multi_image) {
    if (!is.null(attr(classimg[[1]], "px_scale"))) {
      xscale <- lapply(1:length(classimg), function(x) attr(classimg[[x]], "px_scale") * dim(classimg[[x]])[2])
    } else if (is.null(attr(classimg[[1]], "px_scale")) && !is.null(xscale)) {
      xscale <- as.list(rep(xscale, length(classimg)))
    } else if (is.null(attr(classimg[[1]], "px_scale")) && is.null(xscale)) {
      stop("Required argument xscale is missing, and one or more images are uncalibrated.
           Either specify xscale or use procimg() to set a scale for each image.")
    }
  }

  ## Sampling density
  if (!multi_image) {
    if (is.null(xpts)) {
      xpts <- ncol(classimg)
    }
  } else if (multi_image) {
    if (!is.null(xpts)) {
      xpts <- as.list(rep(xpts, length(classimg)))
    }
    if (is.null(xpts)) {
      xpts <- lapply(1:length(classimg), function(x) ncol(classimg[[x]]))
    }
  }

  if (multi_image) { # Multiple images

    imgsize <- format(object.size(classimg), units = "Mb")

    ifelse(imgsize < 100,
      outdata <- pbmclapply(1:length(classimg), function(x) adjacent_main(classimg[[x]],
          xpts_i = xpts[[x]],
          xscale_i = xscale[[x]],
          bkgID_i = bkgID,
          exclude2_i = exclude2,
          coldists_i = coldists
        ),
      mc.cores = cores
      ),
      outdata <- lapply(1:length(classimg), function(x) adjacent_main(classimg[[x]],
          xpts_i = xpts[[x]],
          xscale_i = xscale[[x]],
          bkgID_i = bkgID,
          exclude2_i = exclude2,
          coldists_i = coldists
        ))
    )

    # Combine output, preserving non-shared columns. Base equivalent of; do.call(dplyr::bind_rows, outdata).
    allNms <- unique(unlist(lapply(outdata, names)))
    outdata <- do.call(rbind, c(lapply(outdata, function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)), function(y) NA)))), make.row.names = FALSE))

    for (i in 1:nrow(outdata)) {
      rownames(outdata)[i] <- attr(classimg[[i]], "imgname")
    }
  } else if (!multi_image) { # Single image

    outdata <- adjacent_main(
      classimg_i = classimg,
      xpts_i = xpts,
      xscale_i = xscale,
      bkgID_i = bkgID,
      exclude2_i = exclude2,
      coldists_i = coldists
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
#' @param xpts_i (required) an integer specifying the number of sample points, or grid-sampling
#' density, along the x axis. Y-axis sampling density is calculated automatically
#' from this, to maintain an even grid spacing.
#' @param xscale_i (required) an integer specifying the true length of the x-axis,
#' in preferred units. Not required, and ignored, if image scales have been set via
#' \code{\link{procimg}}.
#' @param exclude2_i The portion of the image to be excluded from the analysis, if any.
#' If excluding the focal object, its outline must first have been idenfitied using 
#' \code{\link{procimg}}. If excluding the image background it must either have been
#' identified using \code{\link{procspec}}, or if it is relatively homogeneous, 
#' the colour-class ID's corresponding to the background can be specified using 
#' bkgID.
#' @param bkgID_i an integer or vector specifying the colour-class ID number(s) of
#' pertaining to the background. Examine the attributes of, or call \code{summary} on,
#' the result of \code{\link{classify}} to visualise the RGB values corresponding to
#' colour-class ID numbers.
#' @param coldists_i An data.frame or matrix specifying the visually-modelled chromatic (dS)
#' and/or achromatic (dL) distances between colour-categories. The first two columns
#' should specify all possible combinations of colour category ID's, with the remaining
#' columns named dS (for chromatic distances) and/or dL (for achromatic distances).
#'
#' @keywords internal
#'
#' @importFrom stats na.omit aggregate
#' @importFrom utils head tail
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
adjacent_main <- function(classimg_i, xpts_i = NULL, xscale_i = NULL, bkgID_i = NULL, exclude2_i = TRUE, coldists_i = NULL) {
  c1 <- c2 <- NULL

  # Scales
  y_scale <- xscale_i / (ncol(classimg_i) / nrow(classimg_i))

  # Simple or 'complex' background?
  bkgoutline <- ifelse(is.null(attr(classimg_i, "outline")),
    FALSE,
    TRUE
  )

  # Subsample (does nothing if x_pts = ncols, default)
  subclass <- classimg_i[
    seq(1, nrow(classimg_i), ncol(classimg_i) / xpts_i),
    seq(1, ncol(classimg_i), ncol(classimg_i) / xpts_i)
  ]

  # Exclude selection, if specified
  if ('background' %in% exclude2_i) {
    # Complex backgrounds
    if (bkgoutline == TRUE) {
      # NA everything *outside* the outlined polyogn
      subclass <- polymask(classimg_i, attr(classimg_i, "outline"), "outside")
      # Subset matrix to include only rows with at least one non-NA transition
      subclass <- subclass[rowSums(!is.na(subclass)) > 1, colSums(!is.na(subclass)) > 1]
    } else {
      # bkgID-based version
      for (i in 1:length(bkgID_i)) {
        subclass[subclass == bkgID_i[[i]]] <- NA
      }
      # Subset matrix to include only rows with at least one non-NA transition
      subclass <- subclass[rowSums(!is.na(subclass)) > 1, colSums(!is.na(subclass)) > 1]
    }
  }
  if ('object' %in% exclude2_i) {
    # Complex backgrounds only
    if (bkgoutline == TRUE) {
      # NA everything *inside* the outlined polyogn
      subclass <- polymask(classimg_i, attr(classimg_i, "outline"), "inside")
      # Subset matrix to include only rows with at least one non-NA transition
      subclass <- subclass[rowSums(!is.na(subclass)) > 1, colSums(!is.na(subclass)) > 1]
    }
  }

  # Summary info
  pt_scale <- xscale_i / xpts_i # distance between grid sample points in user-specified units
  n_x <- ncol(subclass) # n columns
  n_y <- nrow(subclass) # n rows
  # n_area <- sum(rowSums(!is.na(subclass))) # total number of non-NA pixels
  n_class <- length(na.omit(unique(c(as.matrix((subclass)))))) # n color classes
  freq <- as.data.frame(table(as.matrix(subclass))) # raw class frequencies
  freq$rel_freq <- freq$Freq / sum(freq$Freq) # proportion class frequency

  # Single colour check
  single_col <- ifelse(n_class == 1, TRUE, FALSE)

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

  if (single_col) { # TODO: Fix this evil hack
    k <- n_class
    N <- sum(transitions$N)
    n_off <- m_r <- m_c <- m <- t <- 0
    A <- Obs <- E <- d_t_o <- d_t_r <- St <- Jt <- B <- Rt <- Rab <- m_dS <- s_dS <- cv_dS <- m_dL <- s_dL <- cv_dL <- NA

    p <- data.frame(t(freq$rel_freq))
    names(p) <- paste0("p_", freq$Var1)

    q <- data.frame(t(transitions$N / sum(transitions$N)))
    names(q) <- paste0("q_", transitions$c1, "_", transitions$c2)

    Sc <- 1 / sum(freq$rel_freq^2)
    Jc <- Sc
  } else {

    # n colour classes
    k <- n_class

    # Grand total transitions
    N <- sum(transitions$N)

    # Total off-diagonal (class-change) transitions
    n_off <- sum(offdiag$N)

    # Row transition density (mean transitions / row)
    if (nrow(subset(rowtrans2, c1 != c2)) == 0) {
      m_r <- Inf
    } else {
      m_r <- (sum(subset(rowtrans2, c1 != c2)["N"]) / n_y) / xscale_i
    }

    # Col transition density (mean transitions / scaled unit)
    if (nrow(subset(coltrans2, c1 != c2)) == 0) {
      m_c <- Inf
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
      offdiag$exp[i] <- 2 * n_off * freq$rel_freq[freq$Var1 == offdiag[i, 1]] * freq$rel_freq[freq$Var1 == offdiag[i, 2]]
    }
    E <- data.frame(t(offdiag$exp))
    names(E) <- paste0("E_", offdiag$c1, "_", offdiag$c2)

    # Deviations
    d_t_o <- sum(abs(offdiag$N - offdiag$exp)) # observed

    d_t_r <- sum(abs(offdiag$N - offdiag$exp)) # permuted

    # Permutation test
    L <- t(cumsum(t(p))) # Cumulative probabilities

    # permutator <- function(){
    # randpair <- cbind(sample(c(1:k), size = N, replace = TRUE, prob = p), sample(c(1:k), size = N, replace = TRUE, prob = p))
    # randpair <- t(apply(randpair, 1, sort))
    # perm <- subset(aggregate(randpair[,1] ~ ., randpair, FUN = length), V1 != V2)
    # names(perm) <- c('c1', 'c2', 'N')
    # perm$exp <- NA
    # for (i in 1:nrow(perm)) {
    #   perm$exp[i] <- 2 * sum(perm[,3]) * freq$rel_freq[perm[i, 1]] * freq$rel_freq[perm[i, 2]]
    # }
    # sum(abs(perm$N - perm$exp))  # permuted
    # }
    # permutated <- unlist(pbmclapply(1:n.boot, function(x) foo(), mc.cores = cores))

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
    if('background' %in% exclude2_i){
      if (bkgoutline){
        B <- Rt <- Rab <- NA  # TODO
      }else if (!bkgoutline) {
  
        # Animal/background transition ratio
        O_a_a <- offdiag[!offdiag$c1 %in% bkgID_i, ]
        O_a_a <- O_a_a[!O_a_a$c2 %in% bkgID_i, ]
        subb <- paste(offdiag$c1, offdiag$c2, sep = ":") %in% paste(O_a_a[1], O_a_a[2], sep = ":")
        O_a_b <- offdiag[!subb, ]
        B <- sum(O_a_a$N) / sum(O_a_b$N)
  
        # Animal/background transition diversity ratios Rt & Rab
        q_a_a <- q
        q_a_b <- q
        q_b_b <- q
        animID <- setdiff(1:k, bkgID_i)
        for (i in 1:length(bkgID_i)) {
          q_a_a <- q_a_a[, !grepl(bkgID_i[i], names(q_a_a))] # Animal background transitions
          q_b_b <- q_b_b[, !grepl(animID[i], names(q_b_b))] # Background background transitions
        }
        q_a_a <- q_a_a / sum(q_a_a)
        q_b_b <- q_b_b / sum(q_b_b)
  
        Rt <- (1 / sum(q_a_a^2)) / (1 / sum(q_a_b^2))
        Rab <- (1 / sum(q_a_a^2)) / (1 / sum(q_b_b^2))
    }
      }else {
      B <- Rt <- Rab <- NA
    }

    # Boundary strength (Endler et al. 2018)
    if (!is.null(coldists_i)) {
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
    } else {
      m_dS <- s_dS <- cv_dS <- m_dL <- s_dL <- cv_dL <- NA
    }
  }

  # Output
  fin <- data.frame(k, N, n_off, Obs, E, d_t_o, p, q, t, m, m_r, m_c, A, B, Sc, St, Jc, Jt, Rt, Rab, m_dS, s_dS, cv_dS, m_dL, s_dL, cv_dL)
  # fin <- Filter(function(x)!all(is.na(x)), fin)  # Ditch columns that are all NA

  fin <- as.data.frame(fin)

  fin
}


#' Manipulate classified image data that fall inside/outside a polygon
#' 
#' @param imagedat data.
#' @param poly xy polygon coordinates.
#' @param alterWhich manipulate values inside or outside the polygon.
#' 
#' @importFrom sp point.in.polygon
#' 
#' @keywords internal
#' 
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' 
polymask <- function(imagedat, poly, alterWhich = c("inside", "outside")) {
  imglong <- data.frame(expand.grid(1:ncol(imagedat), 1:nrow(imagedat)), z = c(imagedat))
  names(imglong) <- c("x", "y", "z")

  inpoly <- point.in.polygon(imglong$x, imglong$y, poly$x, poly$y, mode.checked = FALSE) # todo: replace with base

  maskmat <- matrix(data = inpoly, ncol(imagedat), nrow(imagedat))
  maskmat <- apply(as.matrix(maskmat), 1, rev)
  if (alterWhich == "inside") {
    imagedat[which(maskmat == 1)] <- NA
    imagedat[which(maskmat == 2)] <- NA
    imagedat[which(maskmat == 3)] <- NA
  }
  if (alterWhich == "outside") {
    imagedat[which(maskmat == 0)] <- NA
  }
  imagedat
}
