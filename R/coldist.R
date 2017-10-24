#' Color distances
#' 
#' Calculates color distances. When data are the result of \code{\link{vismodel}}, 
#' it applies the receptor-noise model of Vorobyev et al. (1998) to calculate color distances
#' with noise based on relative photoreceptor densities. It also accepts \code{\link{colspace}} data 
#' from the hexagon, colour-opponent-coding, categorical, segment, and cielab models, in which case euclidean
#' distances (hexagon, cielab, categorical, segment) or manhattan distances (coc) are returned.
#' 
#' @param modeldata (required) quantum catch color data. Can be the result
#'  from \code{\link{vismodel}}, or \code{\link{colspace}}. Data may also be independently calculated quantum catches, 
#'  in the form of a data frame with columns representing photoreceptors.
#' @param qcatch if the object is of class \code{vismodel} or \code{colspace}, 
#'  this argument is ignored. If the object is a data frame of quantal catches 
#'  from another source, this argument is used to specify what type of quantum catch is being 
#'  used, so that the noise can be calculated accordingly: 
#' \itemize{
#'  \item \code{Qi}: Quantum catch for each photoreceptor
#'  \item \code{fi}: Quantum catch according to Fechner law (the signal of the receptor
#'    channel is proportional to the logarithm of the quantum catch)
#'  }
#' @param subset If only some of the comparisons should be returned, a character vector of 
#'  length 1 or 2 can be provided, indicating which samples are desired. The subset vector 
#'  must match the labels of the input samples, but partial matching (and regular expressions) 
#'  are supported.
#' @param achro Logical. If \code{TRUE}, last column of the data frame is used to calculate 
#'  the achromatic contrast, with noise based on the Weber fraction given by the argument
#'  \code{weber.achro}.
#'  If the data are from the hexagon model (i.e. \code{colspace(space = 'hexagon')}), it 
#'  instead returns simple long (or 'green') receptor contrast.
#' @param n photoreceptor densities for the cones used in visual modeling.
#'  must have same length as number of columns (excluding achromatic receptor if used;
#'  defaults to 
#'  the Pekin robin \emph{Leiothrix lutea} densities: \code{c(1,2,2,4)}). 
#'  Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#'  categorical, and cie models).
#' @param weber The Weber fraction to be used (often also referred to as receptor noise, 
#'  or \emph{e}). The noise-to-signal ratio \code{v} is unknown, 
#'  and therefore must be calculated based on the empirically estimated Weber 
#'  fraction of one of the cone classes. \code{v} is then applied to estimate the 
#'  Weber fraction of the other cones. by default, the value of 0.1 is used 
#'  (the empirically estimated value for the
#'  LWS cone from \emph{Leiothrix lutea}). See Olsson et al. 2017 for a review of 
#'  published values in the literature. Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#'  categorical, segment, and cie models).
#' @param weber.ref the cone class used to obtain the empirical estimate of the 
#'  Weber fraction used for the \code{weber} argument. By default, \code{n4} is used, 
#'  representing the LWS cone for \emph{Leiothrix lutea}. Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, segment, and cie models).
#' @param weber.achro the Weber fraction to be used to calculate achromatic contrast, when 
#'  \code{achro = TRUE}. Defaults to 0.1. Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, segment, and cie models).
#' @param noise how the noise will be calculated. (Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, segment, and cie models)):
#' \itemize{
#' 	\item \code{neural}: noise is proportional to the Weber fraction and 
#'  is independent of the intensity of the signal received (i.e. assumes bright conditions).
#' 	\item \code{quantum}: noise is the sum of the neural noise and receptor noise, 
#'  and is thus proportional to the Weber fraction and inversely proportional 
#'  to the intensity of the signal received (the quantum catches). 
#'  Note that the \code{quantum} option will only work with 
#' 	objects of class \code{vismodel}.
#' }
#' 
#' @param n1,n2,n3,n4,v deprecated arguments. see below.
#'
#' @return A data frame containing up to 4 columns. 
#' The first two (\code{patch1, patch2}) refer
#' to the two colors being contrasted; \code{dS} is the chromatic contrast (delta S)
#' and \code{dL} is the achromatic contrast (delta L). Units are JND's in the receptor-noise
#' model, euclidean distances in the categorical and segment space, manhattan distances in the 
#' color-opponent-coding space, green-receptor contrast in the hexagon, and lightness (L) 
#' contrast in the cielab model. 
#'
#' @section Note on previous versions:
#' previous versions of \code{coldist} calculated receptor noise using the arguments
#' \code{v} for the individual cone noise-to-signal ratio and \code{n1,n2,n3,n4} for
#' the relative cone densities. These arguments have been replaced by \code{weber} and 
#' \code{n}, which takes a vector of relative cone densities. \code{weber.ref} allows
#' the user to specify which receptor to use as the reference to obtain the 
#' desired Weber fraction, and \code{coldist} calculates internally the value of \code{v}
#' to be used when calculating the Weber fraction for the remaining cones. 
#'
#' This allows a more explicit choice of Weber fraction, without the need to find the 
#' right value of \code{v} to use in order to obtain the desired signal-to-noise ratio. Furthermore,
#' by allowing \code{n} to be entered as a vector, \code{coldist} can now handle visual
#' systems with more than four photoreceptors.
#'
#' In addition, the achromatic noise is calculated based on the \code{weber.achro} 
#' argument directly, and not based on \code{v} and \code{n4} as before.
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Dichromat
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis', relative = FALSE)
#' didist.flowers <- coldist(vis.flowers)
#' 
#' # Trichromat 
#' vis.flowers <- vismodel(flowers, visual = 'apis', relative = FALSE)
#' tridist.flowers <- coldist(vis.flowers)
#' 
#' # Trichromat, color-hexagon model (euclidean distances)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', 
#'                         relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colspace(vis.flowers, space = 'hexagon')
#' hexdist.flowers <- coldist(hex.flowers)
#' 
#' # Trichromat, color-opponent-coding model (manhattan distances)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE)
#' coc.flowers <- colspace(vis.flowers, space = 'coc')
#' hexdist.flowers <- coldist(coc.flowers)
#' 
#' # Tetrachromat
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv', relative = FALSE)
#' tetradist.sicalis.n <- coldist(vis.sicalis)
#'
#' # This will also work, but give you several warnings you shouldn't ignore!!
#' col.sicalis <- colspace(vis.sicalis)
#' tetradist.sicalis.n <- coldist(col.sicalis)
#'
#' tetradist.sicalis.q <- coldist(vis.sicalis, noise = 'quantum')
#' }
#' 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' 
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. 
#'  (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative 
#'  Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress 
#'  In Retinal And Eye Research, 20(5), 675-703.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns 
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.
#' @references Olsson, P., Lind, O., & Kelber, A. (2015) Bird colour vision: 
#'  behavioural thresholds reveal receptor noise. Journal of Experimental Biology, 
#'  218, 184-193.
#' @references Lind, O. (2016) Colour vision and background adaptation in a passerine 
#'  bird, the zebra finch (Taeniopygia guttata). Royal Society Open Science, 3, 160383.


coldist <-function(modeldata,
                  noise = c('neural','quantum'), subset = NULL,
                  achro = FALSE, qcatch = NULL,
                  n = c(1,2,2,4), weber = 0.1, weber.ref = 'longest', weber.achro = 0.1,
                  v, n1, n2, n3, n4){
                  	
##################################
# START RECEPTOR NOISE FUNCTIONS #
##################################

newreceptornoise.neural <- function(dat, n, weber, weber.ref, res){
	
  reln <- n/sum(n)
  v <- weber*sqrt(reln[weber.ref])
  e <- setNames(v/sqrt(reln), colnames(dat))

  #############
  # NUMERATOR #
  #############

  # all n-2 combinations (first part numerator)
  n1combs <- combn(colnames(dat),dim(dat)[2]-2)

  # get those combinations of ei and prod(ei)^2

  num1 <- setNames(apply(n1combs, 2, function(x) prod(e[x])^2), 
    apply(n1combs, 2, paste, collapse=""))

  # remaining 2 combinations (second part numerator)
  n2combs <- apply(n1combs, 2, function(x) colnames(dat)[ !colnames(dat) %in% x ] )

  # f_d and f_e

  deltaqiqj <- lapply(1:length(num1), function(y) 
    t(apply(res, 1, function(x)
      dat[x[1], n2combs[,y]] - dat[x[2], n2combs[,y]] ))
      )

  names(deltaqiqj) <- apply(n2combs, 2, paste, collapse='')

  # (f_d-f_e)^2

  num2 <- do.call('cbind',lapply(deltaqiqj, function(x) apply(x, 1, function(z) diff(z)^2)))

  # (e_abc)^2*(f_d-f_e)^2

  etimesq <- num2 %*% diag(num1)

  # sum numerator

  numerator <- rowSums(etimesq)

  ###############
  # DENOMINATOR #
  ###############

  # all n-1 combinations
  dcombs <- combn(colnames(dat),dim(dat)[2]-1)

  den <- setNames(apply(dcombs, 2, function(x) prod(e[x])^2), 
    apply(dcombs, 2, paste, collapse=""))

  denominator <- sum(den)

  ###########
  # DELTA S #
  ###########

  sqrt(numerator/denominator)  
  }



newreceptornoise.quantum <- function(dat, n, weber, weber.ref, res, qndat){
	
  reln <- n/sum(n)
  v <- weber*sqrt(reln[weber.ref])
  
  ept1 <- setNames(v^2/reln, colnames(dat))
  ept2 <- 2/t(apply(res, 1, function(x) qndat[x[1], ] + qndat[x[2], ] ))
  e <- sqrt(sweep(ept2, 2, ept1, "+"))


  #############
  # NUMERATOR #
  #############

  # all n-2 combinations (first part numerator)
  n1combs <- combn(colnames(dat),dim(dat)[2]-2)

  # get those combinations of ei and prod(ei)^2

  num1 <- do.call('rbind', lapply(1:dim(res)[1], function(z) 
    apply(n1combs, 2, function(x) prod(e[z,x])^2)))
  colnames(num1) <- apply(n1combs, 2, paste, collapse="")

  # remaining 2 combinations (second part numerator)
  n2combs <- apply(n1combs, 2, function(x) colnames(dat)[ !colnames(dat) %in% x ] )

  # f_d and f_e

  deltaqiqj <- lapply(1:dim(n1combs)[2], function(y) 
    t(apply(res, 1, function(x)
      dat[x[1], n2combs[,y]] - dat[x[2], n2combs[,y]] ))
      )

  names(deltaqiqj) <- apply(n2combs, 2, paste, collapse='')

  # (f_d-f_e)^2

  num2 <- do.call('cbind',lapply(deltaqiqj, function(x) 
    apply(x, 1, function(z) diff(z)^2)))

  # (e_abc)^2*(f_d-f_e)^2

  etimesq <- num2 * num1

  # sum numerator

  numerator <- rowSums(etimesq)

  ###############
  # DENOMINATOR #
  ###############

  # all n-1 combinations
  dcombs <- combn(colnames(dat),dim(dat)[2]-1)

  den <- do.call('rbind', lapply(1:dim(res)[1], function(z) 
    apply(dcombs, 2, function(x) prod(e[z,x])^2)))
  colnames(den) <- apply(dcombs, 2, paste, collapse="")

  denominator <- rowSums(den)

  ###########
  # DELTA S #
  ###########

  sqrt(numerator/denominator)
  }

# achromatic functions

ttdistcalcachro <- function(f1, f2, weber.achro){
        dq1 <- f1[length(f1)]-f2[length(f1)]
        dq1 <- as.numeric(dq1)
        w <- weber.achro
        round(abs(dq1/w), 7)
        }

qn.ttdistcalcachro <- function(f1,f2, qn1, qn2, weber.achro){
        dq1 <- f1[length(f1)]-f2[length(f1)]
        dq1 <- as.numeric(dq1)
        w <- sqrt((weber.achro)^2 + (2/(qn1[length(qn1)]+qn2[length(qn1)])))
        round(abs(dq1/w),7)
    }


################################
# END RECEPTOR NOISE FUNCTIONS #
################################

#########################
# START OTHER DISTANCES #
#########################


# 2d Euclidean distance
euc2d <- function(coord1, coord2){
  as.numeric(round(sqrt(abs(coord1['x'] - coord2['x'])^2 + abs(coord1['y'] - coord2['y'])^2), 7))
}

# 2d Euclidean distance in segment space
seg2d <- function(coord1, coord2){
  as.numeric(round(sqrt(abs(coord1['MS'] - coord2['MS'])^2 + abs(coord1['LM'] - coord2['LM'])^2), 7))
}

# Achromatic contrast in segment space
achroseg <- function(coord1, coord2){
  as.numeric(abs(coord1['B'] - coord2['B']))
}

# Achromatic 'green' receptor contrast in the hexagon
achrohex <- function(coord1, coord2){
  as.numeric(round(coord1['l'] / coord2['l'], 7))
}

# Achromatic contrast in cielab
achrolab <- function(coord1, coord2){
    as.numeric(abs(coord1['L'] - coord2['L']))
}

# 2d Euclidean distances in CIELAB
lab2d <- function(coord1, coord2){
                     as.numeric(round(sqrt(abs(coord1['L'] - coord2['L'])^2 + abs(coord1['a'] - coord2['a'])^2 +
                                         abs(coord1['b'] - coord2['b'])^2), 7))
}

# CIE2000 colour distance for CIELCh (LOLWAT)
cie2000 <- function(coord1, coord2){
  
  # Lightness difference
  dL <- coord2['L'] - coord1['L']
  
  # Mean lightness
  mL <- (coord2['L'] + coord1['L'])/2
  
  # Chroma difference
  dC <- coord2['C'] - coord1['C']
  
  # Mean chroma
  mC <- (coord2['C'] + coord1['C'])/2
  
  # Hue difference
  if(coord1['h'] - coord2['h'] <= 180)
    dh <- coord2['h'] - coord1['h']
  else if(coord1['h'] - coord2['h'] > 180 & coord2['h'] <= coord1['h'])
    dh <- coord2['h'] + coord1['h'] + 360 
  else if(coord1['h'] - coord2['h'] > 180 & coord2['h'] > coord1['h'])
    dh <- coord2['h'] + coord1['h'] - 360
  
  # Mean hue
  if(abs(coord2['h'] - coord1['h']) <= 180)
    mh <- (coord2['h'] + coord1['h'])/2
  else if(abs(coord2['h'] - coord1['h']) > 180 & coord2['h'] + coord1['h'] < 360)
    mh <- (coord2['h'] + coord1['h'] + 360)/2 
  else if(abs(coord2['h'] - coord1['h']) > 180 & coord2['h'] + coord1['h'] >= 360)
    mh <- (coord2['h'] + coord1['h'] - 360)/2
  
  t <- 1 - (0.17 * cos(mh - 30)) + (0.24 * cos(2 * mh)) + (0.32 * cos(3 * mh + 6)) - (0.2 * cos(4 * mh - 63))
    
  sL <- 1 + ((0.17 * (mL - 50)^2) / sqrt(20 + (mL - 50)^2))
  sC <-  1 + 0.045 * mC
  sH <- 1 + 0.015 * mC * t
  
  Rt <- -2 * sqrt(mC^7 / (mC^7 + 25^7)) * sin(60 * exp(-1 * (((mh - 275)/25)^2)))
  
  as.numeric(round(sqrt((dL/sL)^2 + (dC/sC)^2 + (dh/sH)^2 + (Rt * (dC/sC) * (dh/sH)))), 7)
}

# Manhattan distance
bloc2d <- function(coord1, coord2){
  as.numeric(round(abs(coord1['x'] - coord2['x']) + abs(coord1['y'] - coord2['y'])), 7)
}

#######################
# END OTHER DISTANCES #
#######################
  
  # check deprecated arguments
  if(!missing(v))
    stop('argument v is deprecated, please use weber instead. see ?coldist for more information.', call.=FALSE)

  if(!missing(n1) || !missing(n2) || !missing(n3) || !missing(n4))
    stop('arguments n1, n2, n3 and n4 are deprecated, please use n instead. see ?coldist for more information.', call.=FALSE)
  
  
  noise <- match.arg(noise)
  
  lengthn <- as.character(length(n))

  if(noise == 'quantum'){
  	if(!any(c('vismodel', 'colspace') %in% class(modeldata)))
  	  stop('Object must be of class vismodel or colspace to calculate quantum receptor noise model', call.=FALSE)
  }
  
  # Pre-processing for colspace objects
  if('colspace' %in% class(modeldata)){
  	
    qcatch <- attr(modeldata, 'qcatch')
    ncone <- as.character(attr(modeldata, 'conenumb'))
  	
    dat <- as.matrix(modeldata[, sapply(modeldata, is.numeric)])
        
    if(any(c('dispace','trispace','tcs') %in% attr(modeldata, 'clrsp'))){
      # transform or stop if Qi not appropriate
      qcatch <- attr(modeldata, 'qcatch')
      
      if(lengthn != ncone) 
        stop(paste("vector of relative cone densities (", dQuote("n"), ") is different from the number of cones in the visual model data", sep=''), call.=FALSE)
      dat <- as.matrix(modeldata[, names(modeldata) %in% c('u','s','m','l', 'lum')])
      dat <- switch(qcatch, 
  	                fi = dat, 
  	                Qi = log(dat)
  	                )
  	
  	  # quantum catch models need Qi in original scale (not log transformed)
  	  # to calculate the noise. Save as qndat object.
  	  qndat <- switch(qcatch,
  	           Qi = as.matrix(modeldata),
  	           fi = as.matrix(exp(modeldata)) 
  	           )
  	           
    }
      
    if(attr(modeldata, 'relative'))
      warning('Quantum catch are relative, distances may not be meaningful', call.=FALSE)  
  }
  
  # Pre-processing for vismodel objects
  if('vismodel' %in% class(modeldata)){
  	
  	# set achro=FALSE if visual model has achro='none'
  	if(attr(modeldata, 'visualsystem.achromatic') == 'none'){
  	  if(achro){
  	  	warning(paste('achro=TRUE but visual model was calculated with achro=',dQuote('none'),'; achromatic contrast not calculated.'), call.=FALSE)
  	  }
  	  achro <- FALSE
  	}

    # initial checks... 
    if(attr(modeldata, 'qcatch') == 'Ei')
  	  stop('Receptor-nose model not compatible with hyperbolically transformed quantum catches (Ei)', call.=FALSE)
     
    if(attr(modeldata, 'relative'))
      warning('Quantum catch are relative, distances may not be meaningful', call.=FALSE)  

    # save input object...
  	dat <- as.matrix(modeldata)
  	
  	# transform or stop if Qi not appropriate  	
  	qcatch <- attr(modeldata, 'qcatch')

  	dat <- switch(qcatch, 
  	              fi = dat, 
  	              Qi = log(dat)
  	              )
  	
  	# quantum catch models need Qi in original scale (not log transformed)
  	# to calculate the noise. Save as qndat object.
  	qndat <- switch(qcatch,
  	         Qi = as.matrix(modeldata),
  	         fi = as.matrix(exp(modeldata)) 
  	         )
      
    # choose receptor noise model depending on visual system
    ncone <- as.character(attr(modeldata,'conenumb'))
    
    if(lengthn != ncone) 
      stop(paste("vector of relative cone densities (", dQuote("n"), ") has a different length than the number of cones (columns) used for the visual model", sep=''), call.=FALSE)
    
    rownames(dat) <- rownames(modeldata)
    colnames(dat) <- colnames(modeldata)
   
   }
   
  # transformations in case object is neither from colspace or vismodel
  if(!any(c('colspace','vismodel') %in% class(modeldata))){
  	    
  	if(is.null(qcatch)) 
  	  stop('Scale of quantum catches not defined (Qi or fi in argument qcatch).')
        
    dat <- as.matrix(modeldata)
    
  	# Ensure catches are log transformed
  	dat <- switch(qcatch, 
  	              fi = dat, 
  	              Qi = log(dat)
  	)
  	  	
    rownames(dat) <- rownames(modeldata)
    colnames(dat) <- colnames(modeldata)
    
    if(achro==FALSE){
    	  ncone <- dim(dat)[2]
    	  warning(paste("number of cones not specified; assumed to be", ncone), call.=FALSE)
    }
    
    if(achro==TRUE){
    	  ncone <- dim(dat)[2]-1
    	  warning(paste("number of cones not specified; assumed to be", ncone, "(last column ignored for chromatic contrast, used only for achromatic contrast)"), call.=FALSE)
    }
    
  }

  # Prepare output
  pairsid <- t(combn(nrow(dat),2))
  
  res <- as.data.frame(matrix(rownames(dat)[pairsid], 
                    ncol=2, dimnames=list(NULL, c('patch1', 'patch2'))), stringsAsFactors=FALSE)
                    
  res[,'dS'] <- NA
  
  if(achro)
    res[,'dL'] <- NA
  
  #########################
  # Receptor Noise Models #
  #########################
  
  # should be used when:
  # - colspace object: is not hexagon, coc, categorical, ciexyz, cielab, cielch
  # - vismodel object: always
  # - user input data: always
  
  usereceptornoisemodel <- FALSE
  
  # this covers vismodel, user input
  if(is.null(attr(modeldata, 'clrsp'))) usereceptornoisemodel <- TRUE 
  
  # this covers colspace
  if('colspace' %in% class(modeldata)){
  	if(!attr(modeldata, 'clrsp') %in% c('hexagon', 'categorical', 'CIELAB', 'CIELCh', 'coc', 'segment'))
  	  usereceptornoisemodel <- TRUE
  }
  
  if(usereceptornoisemodel){
  	
   dat2 <- dat[, 1:as.numeric(ncone), drop=FALSE]
  
   if(is.numeric(weber.ref) && weber.ref > length(n)) stop(paste("reference cone class for the empirical estimate of the Weber fraction (", dQuote("weber ref"), ") is greater than the length of vector of relative cone densities (", dQuote("n"), ")", sep=''), call.=FALSE)
   
   if(weber.ref == 'longest') weber.ref <- length(n)
   
   if(length(n) != dim(dat2)[2]) stop(paste("vector of relative cone densities (", dQuote("n"), ") has a different length than the number of cones (columns) used for the visual model", sep=''), call.=FALSE)
   
   
   # CREATE REFERENCE OBJECTS FOR CARTESIAN TRANSFORMATION  
   
   refsamp <- min(dim(dat2)[1], as.numeric(ncone)) 

   	visref <- matrix(NA, 
      ncol = as.numeric(ncone), 
      nrow = refsamp + as.numeric(ncone) + 1, 
      dimnames = list(
        c(rownames(dat2)[seq(refsamp)], 
        paste0('jnd2xyzrrf.', c('achro',colnames(dat2)))),
        colnames(dat2)
        )
    )
   
    rrf <- diag(9, as.numeric(ncone))
    rrf[lower.tri(rrf)] <- 0.001
    rrf[upper.tri(rrf)] <- 0.001
   
    rrf <- log(rrf)
      
    visref[seq(refsamp),] <- dat2[seq(refsamp), ]
    visref[refsamp+1, ] <- log(1e-10)
    visref[-seq(refsamp+1),] <- rrf
  
    resref <- as.data.frame(matrix(rownames(visref)[t(combn(nrow(visref),2))], 
                     ncol=2, dimnames=list(NULL, c('patch1', 'patch2'))), stringsAsFactors=FALSE)
    resref[,'dS'] <- NA
    if(achro)
      resref[,'dL'] <- NA
  
   if(noise=='neural'){
   	res[, 'dS'] <- newreceptornoise.neural(dat=dat2, n=n, weber=weber, 
   	  weber.ref=weber.ref, res=res)

   	resref[, 'dS'] <- newreceptornoise.neural(dat=visref, n=n, weber=weber, 
   	  weber.ref=weber.ref, res=resref)
   } 
   
   if(noise=='quantum') {
    qndat2 <- qndat[, 1:as.numeric(ncone)]
    res[, 'dS'] <- newreceptornoise.quantum(dat=dat2, n=n, weber=weber, 
      weber.ref=weber.ref, res=res, qndat = qndat2)    

    qnref <- exp(visref)
    resref[, 'dS'] <- newreceptornoise.quantum(dat=visref, n=n, weber=weber, 
      weber.ref=weber.ref, res=resref, qndat = qnref)    
   }
   
  
  if(achro){

    if(noise =='neural'){
      res[, 'dL'] <- unlist(lapply(seq(nrow(res)), function(x)
        ttdistcalcachro(f1=dat[res[x,1], ], f2=dat[res[x,2], ], 
        weber.achro = weber.achro)
        ))
        
      visref <- cbind(visref, lum=log(1e-10))
      visref[grep('jnd2xyzrrf', rownames(visref), invert=TRUE), 'lum'] <-
        dat[seq(refsamp), dim(dat)[2]]
 
  	  resref[, 'dL'] <- unlist(lapply(seq(nrow(resref)), function(x)
        ttdistcalcachro(f1= visref[resref[x,1], ], f2= visref[resref[x,2], ], 
        weber.achro = weber.achro)
        ))
    }

  	if(noise == 'quantum'){        
      res[, 'dL'] <- unlist(lapply(seq(nrow(res)), function(x)
        qn.ttdistcalcachro(f1=dat[res[x,1], ], f2=dat[res[x,2], ], 
        qn1=qndat[res[x,1], ], qn2=qndat[res[x,2], ], weber.achro = weber.achro)
        ))
      
      visref <- cbind(visref, lum=log(1e-10))
      visref[grep('jnd2xyzrrf', rownames(visref), invert=TRUE), 'lum'] <-
        dat[seq(refsamp), dim(dat)[2]]

      qnref <- exp(visref)
  	  resref[, 'dL'] <- unlist(lapply(seq(nrow(res)), function(x)
        qn.ttdistcalcachro(f1=visref[resref[x,1], ], f2=visref[resref[x,2], ], 
        qn1=qnref[resref[x,1], ], qn2=qnref[resref[x,2], ], weber.achro = weber.achro)
        ))
    }
    
    if(dim(dat)[2] <= as.numeric(ncone))
      warning('achro is set to TRUE, but input data has the same number of columns for sensory data as number of cones in the visual system. There is no column in the data that represents an exclusively achromatic channel, last column of the sensory data is being used. Treat achromatic results with caution, and check if this is the desired behavior.', call.=FALSE)
  }

  }



#######################
# Other Visual Models #
#######################

if('colspace' %in% class(modeldata)){
	
  if(attr(modeldata, 'clrsp') == 'hexagon'){
    res[, 'dS'] <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res[, 'dL'] <- apply(pairsid, 1, function(x) achrohex(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(modeldata, 'clrsp') == 'segment'){
    res[, 'dS'] <- apply(pairsid, 1, function(x) seg2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res[, 'dL'] <- apply(pairsid, 1, function(x) achroseg(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(modeldata, 'clrsp') == 'categorical'){
    res[, 'dS'] <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the categorical model', call.=FALSE)
  }
  
  if(attr(modeldata, 'clrsp') == 'CIELAB'){
    res[, 'dS'] <- apply(pairsid, 1, function(x) lab2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res[, 'dL'] <- apply(pairsid, 1, function(x) achrolab(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(modeldata, 'clrsp') == 'CIELCh'){
    #res[, 'dS'] <- apply(pairsid, 1, function(x) cie2000(dat[x[1], ], dat[x[2], ]))
    res[, 'dS'] <- apply(pairsid, 1, function(x) lab2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res[, 'dL'] <- apply(pairsid, 1, function(x) achrolab(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(modeldata, 'clrsp') == 'coc'){
    res[, 'dS'] <- apply(pairsid, 1, function(x) bloc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the color-opponent-coding space', call.=FALSE)
  }

}

  nams2 <- with(res, unique(c(patch1, patch2)))
  
# Subsetting samples
  
  if(length(subset) > 2){
    stop('Too many subsetting conditions; one or two allowed.', call.=FALSE)
  }
  
  if(length(subset) == 1){
    
    condition1 <- grep(subset, res$patch1)
    condition2 <- grep(subset, res$patch2)
  
    subsamp <- unique(c(condition1, condition2))
    
    res <- res[subsamp, ]	
   }
    
  if(length(subset) == 2){
    condition1 <- intersect(grep(subset[1], res$patch1), 
      grep(subset[2], res$patch2) )
  	
    condition2 <- intersect(grep(subset[2], res$patch1), 
      grep(subset[1], res$patch2) )
  	
    subsamp <- unique(c(condition1, condition2))
    
    res <- res[subsamp, ]	
   row.names(res) <- 1:dim(res)[1]

  }
  
  if(exists('resref', inherits=FALSE))
    attr(res, 'resref') <- resref
    
  attr(res, 'ncone') <- ncone
  attr(res, 'isrnoise') <- usereceptornoisemodel
    
  res
}