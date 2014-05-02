#' Color distances
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate color distances
#' with receptor noise based on relative photoreceptor densities.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#' from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#' with four columns, representing the avian cones).
#' @param qcatch if the object is of class \code{vismodel}, such as one generated using 
#' \code{pavo}, this argument is ignored. If the object is a data frame of quantal catches 
#' from another source, this argument is used to specify what type of quantum catch is being 
#' used, so that the noise can be calculated accordingly: 
#' \itemize{
#' \item \code{Qi}: Quantum catch for each photoreceptor (default)
#' \item \code{fi}: Quantum catch according to Fechner law (the signal of the receptor
#' channel is proportional to the logarithm of the quantum catch)
#' }
#' @param vis visual system phenotype to use in the model:
#' \itemize{
#' \item \code{tetra}: Tetrachromatic color vision (default)
#' \item \code{tri}: Trichromatic color vision
#' \item \code{di}: Dichromatic color vision
# #' \item \code{mono}: Monochromatic vision
#' }
#' @param subset If only some of the comparisons should be returned, a character vector of 
#' length 1 or 2 can be provided, indicating which samples are desired. The subset vector 
#' must match the labels of the imput samples, but partial matching (and regular expressions) 
#' are supported.
#' @param achro logical. If \code{TRUE}, last column of the data frame is used to calculate 
#' the achromatic contrast, with noise based on the Weber fraction calculated using \code{n4}
#' @param n1,n2,n3,n4 tetrachromatic photoreceptor densities for u, s, m & l (default to 
#' the Pekin robin \emph{Leiothrix lutea} densities: 1:2:2:4). If \code{vis} does not equal 
#' \code{'tetra'}, only \code{n1} and \code{n2} (\code{vis='di'}) or 
#' \code{n1}, \code{n2} and \code{n3} (\code{vis='tri'})
#' are used for chromatic contrast (NOTE: \code{n4} is still the value used for the achromatic
#' contrast.)
#' @param v Noise-to-signal ratio of a single cone (defaults to 0.1, so that under
#' the default densities, the Weber fraction for the large cone will be 0.05, as
#' estimated from behavioral experiment with the Pekin robin, \emph{Leiothrix lutea})
#' @param noise how the noise will be calculated:
#' \itemize{
#' 	\item \code{neural}: noise is proportional to the Weber fraction and is independent of the
#' 	intensity of the signal received.
#' 	\item \code{quantum}: noise is the sum of the neural noise and receptor noise, and is thus
#' 	proportional to the Weber fraction and inversely proportional to the intensity of the signal
#' 	received (the quantum catches). Note that the \code{quantum} option will only work with 
#' 	objects of class \code{vismodel}.
#' }
#'
#' @return A data frame containing 4 columns. The first two (\code{patch1, patch2}) refer
#' to the two colors being contrasted; \code{dS} is the chromatic contrast (delta S, in JNDs)
#' and \code{dL} is the achromatic contrast (delta L, in JNDs)
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv', relative=FALSE)
#' coldist.sicalis <- coldist(vis.sicalis, vis='tetra')}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress In Retinal And Eye Research, 20(5), 675-703.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.


coldist <-function(vismodeldata, qcatch=c('Qi','fi'),
#                  vis=c('tetra', 'tri', 'di', 'mono'), 
                  vis=c('tetra', 'tri', 'di'), 
                  noise=c('neural','quantum'), subset=NULL,
                  achro=TRUE,
                  n1=1, n2=2, n3=2, n4=4, v=0.1)
{

if('vismodel' %in% class(vismodeldata)){
	dat <- as.matrix(vismodeldata)
	qcatch <- attr(vismodeldata, 'qcatch')
#	colnames(dat) <- gsub(paste(qcatch,'.',sep=''),'',colnames(dat))
	
  if(attr(vismodeldata, 'qcatch') == 'Qi')
    qndat <- as.matrix(vismodeldata)

  if(attr(vismodeldata, 'qcatch') == 'fi')
    qndat <- as.matrix(exp(vismodeldata))

  if(attr(vismodeldata,'relative'))
    warning('Quantum catch are relative, distances may not be meaningful')
	
  }else{
	qcatch <- match.arg(qcatch)
  	dat <- as.matrix(vismodeldata)
  	rownames(dat) <- rownames(vismodeldata)
  	colnames(dat) <- colnames(vismodeldata)
  	
  	}

if(!'vismodel' %in% class(vismodeldata) && noise=='quantum')
  stop('Object must be of class vismodel to calculate quantum noise model')

noise <- match.arg(noise)

vis <- match.arg(vis)

dat <- switch(qcatch, fi = dat, Qi = log(dat))

#NEURAL NOISE MODEL

w1e <- v/sqrt(n1)
w2e <- v/sqrt(n2)
w3e <- v/sqrt(n3)
w4e <- v/sqrt(n4)

pairsid <- t(combn(nrow(dat),2))

patch1 <- row.names(dat)[pairsid[,1]]
patch2 <- row.names(dat)[pairsid[,2]]

res <- data.frame(patch1, patch2)

# if (vis=='mono' & noise=='neural'){
# res$dS <- apply(pairsid,1,function(x) 
  # monodistcalc(dat[x[1],], dat[x[2],], 
  # w1=w1e) )
# }


if (vis=='di' & noise=='neural'){
res$dS <- apply(pairsid,1,function(x) 
  didistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e) )
}

if (vis=='tri' & noise=='neural'){
res$dS <- apply(pairsid,1,function(x) 
  trdistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e, w3=w3e) )
}

if (vis=='tetra' & noise=='neural'){
res$dS <- apply(pairsid,1,function(x) 
  ttdistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e, w3=w3e, w4=w4e) )
}

if(achro==TRUE & noise=='neural'){
res$dL <- apply(pairsid,1,function(x) 
  ttdistcalcachro(dat[x[1],], dat[x[2],], 
  w=w4e) )
}




# if (vis=='mono' & noise=='quantum'){
# res$dS <- apply(pairsid,1,function(x) 
  # qn.monodistcalc(dat[x[1],], dat[x[2],], 
  # qndat[x[1],], qndat[x[2],], 
  # n1=n1, v=v) )
# }

if (vis=='di' & noise=='quantum'){
res$dS <- apply(pairsid,1,function(x) 
  qn.didistcalc(dat[x[1],], dat[x[2],], 
  qndat[x[1],], qndat[x[2],], 
  n1=n1, n2=n2, v=v) )
}

if (vis=='tri' & noise=='quantum'){
res$dS <- apply(pairsid,1,function(x) 
  qn.trdistcalc(dat[x[1],], dat[x[2],],
  qndat[x[1],], qndat[x[2],], 
  n1=n1, n2=n2, n3=n3, v=v ) )
}

if (vis=='tetra' & noise=='quantum'){
res$dS <- apply(pairsid,1,function(x) 
  qn.ttdistcalc(dat[x[1],], dat[x[2],],
  qndat[x[1],], qndat[x[2],], 
  n1=n1, n2=n2, n3=n3, n4=n4, v=v) )
}

if(achro==TRUE & noise=='quantum'){
res$dL <- apply(pairsid,1,function(x) 
  qn.ttdistcalcachro(dat[x[1],], dat[x[2],], 
  qndat[x[1],], qndat[x[2],], n4=n4, v=v) )
}


nams2 <- with(res, unique(c(as.character(patch1), as.character(patch2))))

# Subsetting samples

if(length(subset) > 2){
  stop('Too many subsetting conditions; one or two allowed.')
}

if(length(subset)==1){
  
  condition1 <- grep(subset, res$patch1)
  condition2 <- grep(subset, res$patch2)

  subsamp <- unique(c(condition1, condition2))
  
  res <- res[subsamp,]	
 }
  
if(length(subset)==2){
  condition1 <- intersect(grep(subset[1], res$patch1), 
    grep(subset[2],res$patch2) )
	
  condition2 <- intersect(grep(subset[2], res$patch1), 
    grep(subset[1],res$patch2) )
	
  subsamp <- unique(c(condition1, condition2))
  
  res <- res[subsamp,]	
}

row.names(res) <- 1:dim(res)[1]

res
}
