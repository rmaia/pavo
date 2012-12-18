#' Color distances
#' 
#' Applies the visual models of Vorobyev et al. (1998) to calculate color distances
#' with receptor noise based on relative photoreceptor densities.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#' from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#' with four columns, representing the avian cones).
#' @param qcatch quantum catch values to use in the model:
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
#' }
#' @param achro logical. If \code{TRUE}, last column of the data frame is used to calculate 
#' the achromatic contrast, with noise based on the Weber fraction calculated using \code{n4}
#' @param n1,n2,n3,n4 tetrachromatic photoreceptor densities for u, s, m & l (default to 
#' blue tit \emph{Cyanistes caeruleus} densities: 1:2:2:4). If \code{vis} does not equal \code{'tetra'}, only \code{n1}
#' and \code{n2} (\code{vis='di'}) or \code{n1}, \code{n2} and \code{n3} (\code{vis='tri'})
#' are used for chromatic contrast (NOTE: \code{n4} is still the value used for the achromatic
#' contrast.)
#' @param v Noise-to-signal ratio of a single cone (defaults to 0.1, so that under
#' the default densities, the Weber fraction for the large cone will be 0.05, as
#' estimated from behavioral experiment with the Perkin robin, \emph{Leiothrix lutea})
#' @param noise how the noise will be calculated:
#' \itemize{
#' 	\item \code{neural}: noise is proportional to the Weber fraction and is independent of the
#' 	intensity of the signal received.
#' 	\item \code{quantum}: noise is the sum of the quantum noise and the neural noise, and is thus
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
#' coldist.sicalis <- coldist(vis.sicalis, qcatch='fi', vis='tetra')}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I. (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress In Retinal And Eye Research, 20(5), 675-703.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

#ToDo: Add Neural Noise model
#ToDo: make luminance contrast calculation optional

coldist <-function(vismodeldata, qcatch=c('fi', 'Qi'), 
                  vis=c('tetra', 'tri', 'di'), 
                  noise=c('neural','quantum'), achro=TRUE,
                  n1=1, n2=2, n3=2, n4=4, v=0.1)
{

if(class(vismodeldata)=='vismodel'){
	qcatch <- match.arg(qcatch)
#	dat <- data.frame(vismodeldata[qcatch])
	dat <- as.matrix(data.frame(vismodeldata[qcatch]))
	colnames(dat) <- gsub(paste(qcatch,'.',sep=''),'',colnames(dat))
	
	qndat <- as.matrix(vismodeldata$Qi)
	
	if(attr(vismodeldata,'relative'))
	  warning('Quantum catch are relative, distances may not be meaningful')
	
  }else{
	qcatch <- match.arg(qcatch)
  	dat <- as.matrix(vismodeldata)
  	rownames(dat) <- rownames(vismodeldata)
  	colnames(dat) <- colnames(vismodeldata)
  	}

if(class(vismodeldata)!='vismodel' && noise=='quantum')
  stop('Object must be of class vismodel to calculate quantum noise model')

noise <- match.arg(noise)

vis <- match.arg(vis)

dat <- switch(qcatch,
              Qi = log(dat),
              fi = dat)

#NEURAL NOISE MODEL

w1e <- v/sqrt(n1)
w2e <- v/sqrt(n2)
w3e <- v/sqrt(n3)
w4e <- v/sqrt(n4)


# ToDo: this can be later subset if the user doesn't want all comparisons
pairsid <- t(combn(nrow(dat),2))

patch1 <- row.names(dat)[pairsid[,1]]
patch2 <- row.names(dat)[pairsid[,2]]

res <- data.frame(patch1, patch2)

if (vis=='di' & noise=='neural'){
res$di.dS <- apply(pairsid,1,function(x) 
  didistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e) )
}

if (vis=='tri' & noise=='neural'){
res$tri.dS <- apply(pairsid,1,function(x) 
  trdistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e, w3=w3e) )
}

if (vis=='tetra' & noise=='neural'){
res$tetra.dS <- apply(pairsid,1,function(x) 
  ttdistcalc(dat[x[1],], dat[x[2],], 
  w1=w1e, w2=w2e, w3=w3e, w4=w4e) )
}

if(achro==TRUE & noise=='neural'){
res$dL <- apply(pairsid,1,function(x) 
  ttdistcalcachro(dat[x[1],], dat[x[2],], 
  w=w4e) )
}





if (vis=='di' & noise=='quantum'){
res$di.dS <- apply(pairsid,1,function(x) 
  qn.didistcalc(dat[x[1],], dat[x[2],], 
  qndat[x[1],], qndat[x[2],], 
  n1=n1, n2=n2, v=v) )
}

if (vis=='tri' & noise=='quantum'){
res$tri.dS <- apply(pairsid,1,function(x) 
  qn.trdistcalc(dat[x[1],], dat[x[2],],
  qndat[x[1],], qndat[x[2],], 
  n1=n1, n2=n2, n3=n3, v=v ) )
}

if (vis=='tetra' & noise=='quantum'){
res$tetra.dS <- apply(pairsid,1,function(x) 
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

res
}