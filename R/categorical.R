#' Categorical fly-visual model
#' 
#' Applies the fly categorical colour vision model of Troje (1993)
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with four columns representing a tetrachromatic dipteran viewer).
#' 
#' @return Object of class \code{colspace} consisting of the following columns:
#' @return \code{R7p, R7y, R8p, R8y}: the quantum catch data used to
#'  calculate the remaining variables.
#' @return \code{x, y}: cartesian coordinates in the categorical colour space.
#' @return \code{category}: fly-colour category. One of \code{p-y-}, \code{p-y+}, \code{p+y-}, \code{p+y+}.
#' 
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achro = 'none', relative = TRUE)
#' cat.flowers <- colspace(vis.flowers, space = 'categorical')
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Troje N. (1993). Spectral categories in the learning behaviour
#'  of blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.

categorical <- function(vismodeldata){
  
  dat <- vismodeldata
    
# if object is vismodel:
  if('vismodel' %in% attr(dat, 'class')){
    
    # check if tetrachromat
    if(attr(dat, 'conenumb') < 4)
      stop('vismodel input is not tetrachromatic')
    
    if(attr(dat, 'conenumb') > 4)
      warning('vismodel input is not tetrachromatic, considering first four receptors only')
    
    # check if relative
    if(!attr(dat, 'relative')){
      dat <- dat[,c('u','s','m','l')]/rowSums(dat[,c('u','s','m','l')])
      class(dat) <- class(vismodeldata)
      warning("Quantum catch are not relative, and have been transformed")
      attr(vismodeldata,'relative') <- TRUE
    }
    
  }
  
# if not, check if it has more (or less) than 4 columns
  
  if(!('vismodel' %in% attr(dat, 'class'))){
    
    if(ncol(dat) < 4)
      stop('Input data is not a ',  dQuote('vismodel'), ' object and has fewer than four columns')	
    if(ncol(dat) == 4)
      warning('Input data is not a ', dQuote('vismodel'), ' object; treating columns as standardized quantum catch for ', dQuote('u'),', ',  dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')
    
    if(ncol(dat) > 4)
      warning('Input data is not a ', dQuote('vismodel'), ' object *and* has more than four columns; treating the first four columns as standardized quantum catch for ', dQuote('u'),', ',  dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')
    
    dat <- dat[, 1:4]
    
    if(round(sum(rowSums(dat/apply(dat,1,sum)))) != dim(dat)[1]){
      dat <- dat/apply(dat, 1, sum)
      warning('Quantum catch are not relative, and have been transformed')
      attr(vismodeldata,'relative') <- TRUE
    }
  }
  
  
  R7p <- dat[, 1]
  R7y <- dat[, 2]
  R8p <- dat[, 3]
  R8y <- dat[, 4]
    
# x & y coordinates
  x <- R7p - R8p
  y <- R7y - R8y
  
# Colour category calculator (surely a better way?)
  colcat <- function(object){
      if(object$x > 0 && object$y > 0)
        return('p+y+')
      if(object$x < 0 && object$y > 0)
        return('p-y+')
      if(object$x < 0 && object$y < 0)
        return('p-y-')
      if(object$x > 0 && object$y < 0)
        return('p+y-')
  }
  
  res.p <- data.frame(R7p, R7y, R8p, R8y, x, y, row.names = rownames(dat))
  
  res.p$category <- sapply(1:nrow(res.p), function(x) colcat(res.p[x,]))
  
  res <- res.p
  
  class(res) <- c('colspace', 'data.frame')
  
  # Descriptive attributes (largely preserved from vismodel)
  attr(res, 'clrsp') <- 'categorical'
  attr(res, 'conenumb') <- 4
  attr(res, 'qcatch') <- attr(vismodeldata, 'qcatch')
  attr(res,'visualsystem.chromatic') <- attr(vismodeldata,'visualsystem.chromatic')
  attr(res,'visualsystem.achromatic') <- attr(vismodeldata,'visualsystem.achromatic')
  attr(res,'illuminant') <- attr(vismodeldata,'illuminant')
  attr(res,'background') <- attr(vismodeldata,'background')
  attr(res,'relative') <- attr(vismodeldata,'relative')
  attr(res, 'vonkries') <- attr(vismodeldata, 'vonkries')
  
  # Data attributes
  attr(res, 'data.visualsystem.chromatic') <- attr(vismodeldata, 'data.visualsystem.chromatic')
  attr(res, 'data.visualsystem.achromatic') <- attr(vismodeldata, 'data.visualsystem.achromatic')
  attr(res, 'data.background') <- attr(vismodeldata, 'data.background')
  
  res
}