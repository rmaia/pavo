#' Color distances
#' 
#' Calculates color distances. When data are the result of \code{\link{vismodel}}, 
#' it applies the receptor-noise model of Vorobyev et al. (1998) to calculate color distances
#' with noise based on relative photoreceptor densities. It also accepts \code{\link{colspace}} data 
#' from the hexagon, colour-opponent-coding, categorical, and cielab models, in which case euclidean
#' distances (hexagon, cielab, categorical) or manhattan distances (coc) are returned.
#' 
#' @param qdata (required) quantum catch color data. Can be the result
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
#' @param vis if the object is of class \code{vismodel} or \code{colspace}, 
#'  this argument is ignored. If the object is a data frame of quantal catches 
#'  from another source, this argument is used to specify the visual system phenotype 
#'  to use in the model:
#' \itemize{
#'  \item \code{tcs}: Tetrachromatic color vision (default)
#'  \item \code{tri}: Trichromatic color vision
#'  \item \code{di}: Dichromatic color vision
#' }
#' @param subset If only some of the comparisons should be returned, a character vector of 
#'  length 1 or 2 can be provided, indicating which samples are desired. The subset vector 
#'  must match the labels of the imput samples, but partial matching (and regular expressions) 
#'  are supported.
#' @param achro Logical. If \code{TRUE}, last column of the data frame is used to calculate 
#'  the achromatic contrast, with noise based on the Weber fraction calculated using \code{n4}. 
#'  If the data are from the hexagon model (i.e. \code{colspace(space = 'hexagon')}), it 
#'  instead returns long (or 'green') receptor contrast.
#' @param n1,n2,n3,n4 tetrachromatic photoreceptor densities for u, s, m & l (default to 
#'  the Pekin robin \emph{Leiothrix lutea} densities: 1:2:2:4). If \code{vis} does not equal 
#'  \code{'tcs'}, only \code{n1} and \code{n2} (\code{vis = 'di'}) or 
#'  \code{n1}, \code{n2} and \code{n3} (\code{vis = 'tri'}) are used for chromatic contrast. 
#'  Ignored when data are of class \code{\link{colspace}}.
#' @param weber The Weber fraction to be used. The noise-to-signal ratio \code{v} is unknown, 
#'  and therefore must be calculated based on the epirically estimated Weber fraction of one of
#'  the cone classes. \code{v} is then applied to estimate the Weber fraction of the 
#'  other cones. by default, the value of 0.1 is used (the empirically estimated value for the
#'  LWS cone from \emph{Leiothrix lutea}). Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, and cielab models).
#' @param weber.ref the cone class used to obtain the empirical estimate of the 
#'  Weber fraction used for the \code{weber} argument. By default, \code{n4} is used, 
#'  representing the LWS cone for \emph{Leiothrix lutea}. Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, and cielab models).
#' @param weber.achro the Weber fraction to be used to calculate achromatic contrast, when 
#'  \code{achro = TRUE}. Defaults to 0.1. Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, and cielab models).
#' @param noise how the noise will be calculated. (Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#' categorical, and cielab models)):
#' \itemize{
#' 	\item \code{neural}: noise is proportional to the Weber fraction and is independent of the
#' 	intensity of the signal received.
#' 	\item \code{quantum}: noise is the sum of the neural noise and receptor noise, and is thus
#' 	proportional to the Weber fraction and inversely proportional to the intensity of the signal
#' 	received (the quantum catches). Note that the \code{quantum} option will only work with 
#' 	objects of class \code{vismodel}.
#' }
#'
#' @return A data frame containing up to 4 columns. The first two (\code{patch1, patch2}) refer
#' to the two colors being contrasted; \code{dS} is the chromatic contrast (delta S)
#' and \code{dL} is the achromatic contrast (delta L). Units are JND's in the receptor-noise
#' model, euclidean distances in the hexagon and cielab colorspaces, and manhattan distances
#' in the color-opponent-coding space. 
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Dichromat
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis', relative= FALSE)
#' didist.flowers <- coldist(vis.flowers)
#' 
#' # Trichromat 
#' vis.flowers <- vismodel(flowers, visual = 'apis', relative = FALSE)
#' tridist.flowers <- coldist(vis.flowers)
#' 
#' # Trichromat, color-hexagon model
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colspace(vis.flowers, space = 'hexagon')
#' hexdist.flowers <- coldist(hex.flowers)
#' 
#' # Tetrachromat
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv', relative = FALSE)
#' tetradist.sicalis.n <- coldist(vis.sicalis)
#' tetradist.sicalis.q <- coldist(vis.sicalis, noise='quantum')
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

coldist <-function(qdata,
                  noise = c('neural','quantum'), subset = NULL,
                  achro = TRUE, vis = NULL, qcatch = NULL,
                  n1 = 1, n2 = 2, n3 = 2, n4 = 4, 
                  weber = 0.1, weber.ref = 'n4', weber.achro = 0.1){
  
  noise <- match.arg(noise)
      
  # Pre-processing for colspace objects
  if('colspace' %in% class(qdata)){
    dat <- as.matrix(qdata[, sapply(qdata, is.numeric)])
    qcatch <- attr(qdata, 'qcatch')
    
    if(any(c('di','tri','tcs') %in% attr(qdata, 'clrsp')))
      vis <- attr(qdata, 'clrsp')
  
    if(attr(qdata, 'relative'))
      warning('Quantum catch are relative, distances may not be meaningful')  
  }

  if(!'vismodel' %in% class(qdata) && noise == 'quantum')
    stop('Object must be of class vismodel to calculate quantum noise model')


  # Pre-processing for vismodel objects
  if('vismodel' %in% class(qdata)){
  	dat <- as.matrix(qdata)
    rownames(dat) <- rownames(qdata)
    colnames(dat) <- colnames(qdata)
  	
  	# transform or stop if Qi not appropriate
  	qcatch <- attr(qdata, 'qcatch')
  	
    if(attr(qdata, 'qcatch') == 'Qi')
      qndat <- as.matrix(qdata)
  
    if(attr(qdata, 'qcatch') == 'fi')
      qndat <- as.matrix(exp(qdata))
  	
  	if(attr(qdata, 'qcatch') == 'Ei')
  	  stop('Receptor-nose model not compatible with hyperbolically transformed quantum catches')
     
    if(attr(qdata, 'relative'))
      warning('Quantum catch are relative, distances may not be meaningful')  
      
    # choose receptor noise model depending on visual system
    ncone <- as.character(attr(qdata,'conenumb'))
    vis <- switch(ncone,
                  '2' = 'di',
                  '3' = 'tri',
                  '4' = 'tcs')
    
  }
  
# RM: is this supposed to be in case it's neither colspace nor vismodel?
#  if(!'colspace' %in% class(qdata)){
  if(!any(c('colspace','vismodel') %in% class(qdata))){
    vis <- match.arg(vis)
    dat <- switch(qcatch, fi = dat, Qi = log(dat))
  }
  
  
  # Pair up stimuli
  pairsid <- t(combn(nrow(dat), 2))
  patch1 <- row.names(dat)[pairsid[, 1]]
  patch2 <- row.names(dat)[pairsid[, 2]]
  res <- data.frame(patch1, patch2)
  
  ### Receptor-noise models ###
  if(!is.null(vis)){
    # Calculate v based on weber fraction and reference cone
    v <- switch(weber.ref,
            n1 = weber * sqrt(n1),
            n2 = weber * sqrt(n2),
            n3 = weber * sqrt(n3),
            n4 = weber * sqrt(n4))
    
    # Neural noise
    w1e <- v/sqrt(n1)
    w2e <- v/sqrt(n2)
    w3e <- v/sqrt(n3)
    w4e <- v/sqrt(n4)
    
    if(vis == 'di' & noise == 'neural'){
    res$dS <- apply(pairsid, 1, function(x) 
      didistcalc(dat[x[1], ], dat[x[2], ], 
      w1 = w1e, w2 = w2e) )
    }
    
    if(vis == 'tri' & noise == 'neural'){
    res$dS <- apply(pairsid, 1, function(x) 
      trdistcalc(dat[x[1], ], dat[x[2], ], 
      w1 = w1e, w2 = w2e, w3 = w3e) )
    }
    
    if(vis == 'tcs' & noise == 'neural'){
    res$dS <- apply(pairsid, 1, function(x) 
      ttdistcalc(dat[x[1], ], dat[x[2], ], 
      w1 = w1e, w2 = w2e, w3 = w3e, w4 = w4e) )
    }
    
    if(achro == TRUE & noise == 'neural'){
    res$dL <- apply(pairsid, 1, function(x) 
      ttdistcalcachro(dat[x[1], ], dat[x[2], ], 
      w = weber.achro) )
    }
    
    if (vis == 'di' & noise == 'quantum'){
    res$dS <- apply(pairsid, 1, function(x) 
      qn.didistcalc(dat[x[1], ], dat[x[2], ], 
      qndat[x[1], ], qndat[x[2], ], 
      n1 = n1, n2 = n2, v = v) )
    }
    
    if(vis == 'tri' & noise == 'quantum'){
    res$dS <- apply(pairsid, 1, function(x) 
      qn.trdistcalc(dat[x[1],], dat[x[2], ],
      qndat[x[1], ], qndat[x[2], ], 
      n1 = n1, n2 = n2, n3 = n3, v = v ) )
    }
    
    if(vis == 'tcs' & noise == 'quantum'){
    res$dS <- apply(pairsid, 1, function(x) 
      qn.ttdistcalc(dat[x[1], ], dat[x[2], ],
      qndat[x[1], ], qndat[x[2], ], 
      n1 = n1, n2 = n2, n3 = n3, n4 = n4, v = v) )
    }
    
    if(achro == TRUE & noise == 'quantum'){
    res$dL <- apply(pairsid, 1, function(x) 
      qn.ttdistcalcachro(dat[x[1], ], dat[x[2], ], 
      qndat[x[1], ], qndat[x[2], ], weber = weber.achro) )
    }
    
  }
  
  ### colspace model distances ###
if('colspace' %in% class(qdata)){
	
  if(attr(qdata, 'clrsp') == 'hexagon'){
    res$dS <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res$dL <- apply(pairsid, 1, function(x) achrohex(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(qdata, 'clrsp') == 'categorical'){
    res$dS <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the categorical model')
  }
  
  if(attr(qdata, 'clrsp') == 'CIELAB'){
    res$dS <- apply(pairsid, 1, function(x) lab2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the CIELAB model')
  }
  
  if(attr(qdata, 'clrsp') == 'coc'){
    res$dS <- apply(pairsid, 1, function(x) bloc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the color-opponent-coding space')
  }

}
  
  nams2 <- with(res, unique(c(as.character(patch1), as.character(patch2))))
  
  # Subsetting samples
  if(length(subset) > 2){
    stop('Too many subsetting conditions; only one or two allowed.')
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
  }
  
  row.names(res) <- 1:dim(res)[1]
  
  res
}
