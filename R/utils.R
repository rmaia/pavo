copy_attributes <- function(target, source, which) {
  for (attr_name in which) {
    attr(target, attr_name) <- attr(source, attr_name)
  }

  return(target)
}
