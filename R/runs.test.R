##
##  Wald Wolfowitz Runs Test
##
runs.test <- function(x, alternative="two.sided", level=median(x), pvalue="normal"){
  # Performs the Runs Test for Randomness.
  #
  # Args:
  #   x: a numeric vector containing the data.
  #   alternative hypothesis, must be one of "two.sided" (default), "left.sided" or "right.sided"
  #   level: 
  #
  # Returns:
  #   statistic: the (normalized) value of the statistic test.
  #   n: the sample size, after the remotion of consecutive duplicate values.
  #   p.value: the asymptotic p-value.
  #
  dname <- deparse(substitute(x))
  # Remove NAs
  x<-na.omit(x)
  stopifnot(is.numeric(x))
  #n <- length(x)
  x <- x - level
  # Remove values equal to the level
  x<-x[x!=0]
  s <- sign(x)
  n1 <- length(s[s>0]) 
  n2 <- length(s[s<0])
  runs <- rle(s)
  r1 <- length(runs$lengths[runs$values==1])
  r2 <- length(runs$lengths[runs$values==-1])  
  n<-n1+n2
  mu <- 1 + 2*n1*n2/(n1+n2)
  vr <- 2*n1*n2*(2*n1*n2-n1-n2)/(n^2*(n-1))
  rr <- r1+r2
  
  # Computes the p-value
  if (pvalue == "exact"){
  }
  if (pvalue=="normal"){ 
    #RVN <- (RVN - mu) / sqrt(vr)
    pv0 <- pnorm((rr - mu) / sqrt(vr))
  }  
  if (alternative=="two.sided"){pv <- 2*min(pv0,1-pv0); alternative<-"randomness"}
  if (alternative=="left.sided"){pv <- pv0; alternative<-"trend"}
  if (alternative=="right.sided") {pv <- 1-pv0; alternative<-"first-order negative autocorrelation"}
  
  names(mu)="mu"
  names(vr)="vr"
  names(n1)="n1"
  names(n2)="n2"
  names(n)="n"
  rval <- list(statistic = c(statistic=rr, mu, vr), p.value = pv, 
               method = "Runs Test", data.name = dname, parameter=c(n1,n2,n), n1=n1, n2=n2, n=n, alternative=alternative)  
  class(rval) <- "htest"
  return(rval)
  
}  

# x<-c(68, 71, 69, 71, 70, 65, 63, 64, 65, 64, 67, 68, 66, 68, 66, 70)