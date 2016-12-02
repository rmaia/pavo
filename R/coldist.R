#' Color distances
#' 
#' Calculates color distances. When data are the result of \code{\link{vismodel}}, 
#' it applies the receptor-noise model of Vorobyev et al. (1998) to calculate color distances
#' with noise based on relative photoreceptor densities. It also accepts \code{\link{colspace}} data 
#' from the hexagon, colour-opponent-coding, categorical, and cielab models, in which case euclidean
#' distances (hexagon, cielab, categorical) or manhattan distances (coc) are returned.
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
#'  must match the labels of the imput samples, but partial matching (and regular expressions) 
#'  are supported.
#' @param achro Logical. If \code{TRUE}, last column of the data frame is used to calculate 
#'  the achromatic contrast, with noise based on the Weber fraction given by the argument
#'  \code{weber.achro}.
#'  If the data are from the hexagon model (i.e. \code{colspace(space = 'hexagon')}), it 
#'  instead returns long (or 'green') receptor contrast.
#' @param n photoreceptor densities for the cones used in visual modeling.
#'  must have same length as number of columns (excluding achromatic receptor if used;
#'  defaults to 
#'  the Pekin robin \emph{Leiothrix lutea} densities: \code{c(1,2,2,4)}). 
#'  Ignored for \code{colspace} objects
#'  if model is not a receptor noise model (i.e. hexagon, colour-opponent-coding, 
#'  categorical, and cielab models).
#' @param weber The Weber fraction to be used. The noise-to-signal ratio \code{v} is unknown, 
#'  and therefore must be calculated based on the epirically estimated Weber 
#'  fraction of one of the cone classes. \code{v} is then applied to estimate the 
#'  Weber fraction of the other cones. by default, the value of 0.1 is used 
#'  (the empirically estimated value for the
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
#' model, euclidean distances in the hexagon, cielab, and categorical colorspaces, 
#' and manhattan distances in the color-opponent-coding space. 
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
#' This allows
#' a more explicit choice of Weber fraction, without the need to find the right value of
#' \code{v} to use in order to obtain the desired signal-to-noise ratio. Additionally,
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

coldist <-function(modeldata,
                  noise = c('neural','quantum'), subset = NULL,
                  achro = TRUE, qcatch = NULL,
                  n = c(1,2,2,4), weber = 0.1, weber.ref = 'longest', weber.achro = 0.1,
                  v,n1,n2,n3,n4){
  if(!missing(v))
    stop('argument v is deprecated, please use weber instead. see ?coldist for more information.', call.=FALSE)

  if(!missing(n1) || !missing(n2) || !missing(n3) || !missing(n4))
    stop('arguments n1, n2, n3 and n4 are deprecated, please use n instead. see ?coldist for more information.', call.=FALSE)
   
  noise <- match.arg(noise)
  
  lengthn <- as.character(length(n))

  if(noise == 'quantum'){
  	if(!any(c('vismodel', 'colspace') %in% class(modeldata)))
  	  stop('Object must be of class vismodel or colspace to calculate quantum receptor noise model')
  }
  
  # Pre-processing for colspace objects
  if('colspace' %in% class(modeldata)){
    dat <- as.matrix(modeldata[, sapply(modeldata, is.numeric)])
    
    qcatch <- attr(modeldata, 'qcatch')
    
    if(any(c('dispace','trispace','tcs') %in% attr(modeldata, 'clrsp'))){
      # transform or stop if Qi not appropriate
      qcatch <- attr(modeldata, 'qcatch')
      ncone <- as.character(attr(modeldata,'conenumb'))
      
      if(lengthn != ncone) 
        stop(paste("vector of relative cone densities (", dQuote("n"), ") is different from the number of cones in the visual model data", sep=''))
  

      dat <- as.matrix(modeldata[, names(modeldata) %in% c('u','s','m','l')])
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
      warning('Quantum catch are relative, distances may not be meaningful')  
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
  	  stop('Receptor-nose model not compatible with hyperbolically transformed quantum catches (Ei)')
     
    if(attr(modeldata, 'relative'))
      warning('Quantum catch are relative, distances may not be meaningful')  

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
      stop(paste("vector of relative cone densities (", dQuote("n"), ") has a different length than the number of cones (columns) used for the visual model", sep=''))
    
    rownames(dat) <- rownames(modeldata)
    colnames(dat) <- colnames(modeldata)
   
   }
   
  # transformations in case object is neither from colspace or vismodel
  if(!any(c('colspace','vismodel') %in% class(modeldata))){
  	qcatch <- match.arg(qcatch)
    dat <- as.matrix(modeldata)
    rownames(dat) <- rownames(modeldata)
    colnames(dat) <- colnames(modeldata)
    
    if(achro==FALSE){
    	  ncone <- dim(dat)[2]
    	  warning(paste("number of cones not specified; assumed to be", ncone))
    }
    
    if(achro==TRUE){
    	  ncone <- dim(dat)[2]-1
    	  warning(paste("number of cones not specified; assumed to be", ncone, "(last column ignored for chromatic contrast, used only for achromatic contrast)"))
    }
    
  }

  # Prepare output
  pairsid <- t(combn(nrow(dat),2))

  patch1 <- row.names(dat)[pairsid[, 1]]
  patch2 <- row.names(dat)[pairsid[, 2]]

  res <- data.frame(patch1, patch2)
  
  #########################
  # Receptor Noise Models #
  #########################
  
  # should be used when:
  # - colspace object: is not hexagon, coc, categorical, ciexyz, cielab
  # - vismodel object: always
  # - user input data: always
  
  usereceptornoisemodel <- FALSE
  
  # this covers vismodel, user input
  if(is.null(attr(modeldata, 'clrsp'))) usereceptornoisemodel <- TRUE 
  
  # this covers colspace
  if('colspace' %in% class(modeldata)){
  	if(!attr(modeldata, 'clrsp') %in% c('hexagon', 'categorical', 'CIELAB', 'coc'))
  	  usereceptornoisemodel <- TRUE
  }
  
  if(usereceptornoisemodel){
  	
   dat2 <- dat[, 1:as.numeric(ncone)]
  
   if(is.numeric(weber.ref) && weber.ref > length(n)) stop(paste("reference cone class for the empirical estimate of the Weber fraction (", dQuote("weber ref"), ") is greater than the length of vector of relative cone densities (", dQuote("n"), ")", sep=''))
   
   if(weber.ref == 'longest') weber.ref <- length(n)
   
   if(length(n) != dim(dat2)[2]) stop(paste("vector of relative cone densities (", dQuote("n"), ") has a different length than the number of cones (columns) used for the visual model", sep=''))
  
   if(noise=='neural'){
   	res$dS <- newreceptornoise.neural(dat=dat2, n=n, weber=weber, 
   	  weber.ref=weber.ref, res=res)
   } 
   
   if(noise=='quantum') {
    qndat2 <- qndat[, 1:as.numeric(ncone)]
    res$dS <- newreceptornoise.quantum(dat=dat2, n=n, weber=weber, 
      weber.ref=weber.ref, res=res, qndat = qndat2)     	
   }
  
  if(achro == TRUE){
  	if(noise == 'quantum')
  	  res$dL <- apply(pairsid, 1, function(x) 
        qn.ttdistcalcachro(f1=dat[x[1], ], f2=dat[x[2], ], 
        qn1=qndat[x[1], ], qn2=qndat[x[2], ], weber.achro = weber.achro))
   
    if(noise =='neural')
      res$dL <- apply(pairsid, 1, function(x) 
        ttdistcalcachro(f1=dat[x[1], ], f2=dat[x[2], ], weber.achro = weber.achro))
    
    if(dim(dat)[2] <= as.numeric(ncone))
      warning('achro is set to TRUE, but input data has the same number of columns for sensory data as number of cones in the visual system, so there is no column in the data that represents an exclusively achromatic channel. The last column of the sensory data is being used. Treat achromatic results with caution, and check if this is the desired behavior.', call.=FALSE)
    
  }

  }



#######################
# Other Visual Models #
#######################

if('colspace' %in% class(modeldata)){
	
  if(attr(modeldata, 'clrsp') == 'hexagon'){
    res$dS <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      res$dL <- apply(pairsid, 1, function(x) achrohex(dat[x[1], ], dat[x[2], ]))
  }
  
  if(attr(modeldata, 'clrsp') == 'categorical'){
    res$dS <- apply(pairsid, 1, function(x) euc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the categorical model')
  }
  
  if(attr(modeldata, 'clrsp') == 'CIELAB'){
    res$dS <- apply(pairsid, 1, function(x) lab2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the CIELAB model')
  }
  
  if(attr(modeldata, 'clrsp') == 'coc'){
    res$dS <- apply(pairsid, 1, function(x) bloc2d(dat[x[1], ], dat[x[2], ]))
    if(achro == TRUE)
      warning('Achromatic contrast not calculated in the color-opponent-coding space')
  }

}

  nams2 <- with(res, unique(c(as.character(patch1), as.character(patch2))))
  
# Subsetting samples
  
  if(length(subset) > 2){
    stop('Too many subsetting conditions; one or two allowed.')
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