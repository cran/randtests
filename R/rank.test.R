##
##  Mann-Kendall Rank Test
##
rank.test <- function(x, alternative)
{
  stopifnot(is.numeric(x))
  # Remove NAs
  x<-na.omit(x)
  
  dname <- deparse(substitute(x))
  
  n <- length(x)
  p <- 0
  for (i in 1:(n-1)){
    t <- x[(i+1):n]
    p <- p+length(t[t>x[i]])
  }
  mu <- n*(n-1)/4
  vr <- n*(n-1)*(2*n+5)/72
  test <- (p-mu)/sqrt(vr)
  
  return(test)
}
