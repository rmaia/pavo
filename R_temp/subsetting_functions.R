subset.rspec <- function (rspecdata, subset, ...) {
  # if (!is.logical(subset)) 
  #   stop("'subset' must be logical")
  if (!any(class(specs)=="rspec"))
    stop("object not of class 'rspec'")
  subsample <- grep(subset, names(rspecdata))
  res <- cbind(rspecdata["wl"], rspecdata[subsample]) # & !is.na(subset)])
  class(res) <- c("rspec", "data.frame")
  res
}

# head(subset(acut, "rump"))

subset.tcs <- function (tcsdata, subset, ...) {
  # if (!is.logical(subset)) 
  #   stop("'subset' must be logical")
  if (!any(class(specs)=="tcs"))
    stop("object not of class 'tcs'")
  subsample <- grep(subset, row.names(tcsdata))
  res <- tcsdata[subsample, ] # & !is.na(subset)])
  class(res) <- c("tcs", "data.frame")
  res
}

# subset(tcs.star, "wing")
# tcsplot(subset(tcs.star, "wing"))

subset.vismodel <- function (vismodeldata, subset, ...) {
  # if (!is.logical(subset)) 
  #   stop("'subset' must be logical")
  attr <- attributes(vm.star1)
  if (!any(class(vismodeldata)=="vismodel"))
    stop("object not of class 'vismodel'")
  subsample <- grep(subset, row.names(vismodeldata[[1]]))
  res.qi <- vismodeldata$Qi[subsample, ]
  res.fi <- vismodeldata$fi[subsample, ]
  res <- list(Qi=res.qi, fi=res.fi)
  class(res) <- c("vismodel")
  attributes(res) <- attr
  res
}

# subset(vm.star1, "belly")
