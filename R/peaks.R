##THIS FUNCTION TAKES A GIVEN VECTOR OF VALUES AND DETERMINES THE INDICES AT WHICH PEAKS OCCUR. SPAN SETS THE SENSITIVITY (RANGE OF VALUES TO LOOK OVER). TAKEN FROM BRIAN RIPLEY (ONLINE MAILING LIST)
peaks <- function(series,span=3) 
{ 
  z <- embed(series, span) 
  s <- span%/%2 
  v<- max.col(z) == 1 + s 
  result <- c(rep(FALSE,s),v) 
  result <- result[1:(length(result)-s)] 
  result 
}
