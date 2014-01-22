##
##  turning point code
##
turning.point.test <- function(x){
  stopifnot(is.numeric(x))
  # Delete consecutive repeated values
  if (min(diff(x))==0){
    d<-which(diff(x) %in% 0)
    x<-x[-d]
  }
  # Main code  
  n <- length(x)
  mu <- 2*(n-2)/3
  var <- (16*n-29)/90
  
  #ts <- embed(x,3)
  #test.sum <- sum((ts[,2] > ts[,1] & ts[,2] > ts[,3]) | (ts[,2] < ts[,1] & ts[,2] < ts[,3]))
  tp <- sign(apply(embed(x,3), 1, diff))
  tp <- tp[1,]*tp[2,]
  test.sum <- -sum(tp[tp<0])
  test <- (test.sum-mu)/sqrt(var)
  return(test)
  #p.value <- 2*(1-pnorm(test))
  #structure(list(test.sum=test.sum,test=test,p.value=p.value,mu=mu,var=var))
}