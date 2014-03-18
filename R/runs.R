##
##  probability function of the runs statistic
##
druns <- function(x, n1, n2, log = FALSE){
  stopifnot(is.numeric(x))
  r0<-NULL 
  for (i in 1:length(x)){
    if (x[i]>1 & x[i]<=n1+n2 & x[i]==as.integer(x[i])){
      # Check if x[i] is even or odd
      rm<-round(x[i]) %% 2
      if (rm == 0){
        r0 <- c(r0, 2*choose(n1-1, x[i]/2-1)*choose(n2-1, x[i]/2-1))
      }
      else {
        r0 <- c(r0, (choose(n1-1, (x[i]-1)/2)*choose(n2-1, (x[i]-3)/2)+choose(n1-1, (x[i]-3)/2)*choose(n2-1, (x[i]-1)/2)))
      }
    } else {
      r0 <- c(r0, 0)
    }  
   }
  r0<-r0/choose(n1+n2, n1)
# if TRUE, probabilities p are given as log(p).  
ifelse(log,return(log(r0)),return(r0))  
}
##
##  distribution function of the runs statistic
##
pruns <- function(q, n1, n2, lower.tail = TRUE, log = FALSE){
  stopifnot(is.numeric(q) & n1>0 & n2>0)
  r0 <- NULL
  if (lower.tail){
    for (i in 1:length(q)){
      r0 <- c(r0,ifelse(q[i]>=2,sum(druns(x=2:floor(q[i]),n1,n2,log=log)),0))
    }
  }
  else {
    r0 <- 1-pruns(q,n1,n2,lower.tail=T, log=log)
  }  
  return(r0)  
}  
##
##  distribution function of the runs statistic
##
qruns <- function(p, n1, n2, lower.tail = TRUE, log = FALSE){
  r0 <- NULL
  n <- n1+n2
  pr<-c(0,cumsum(druns(2:n, n1, n2)))
  for (i in 1:length(p)){
    if (p[i]>=0 & p[i]<=1){
      #rq<-which(abs(pr-p)==min(abs(pr-p))) 
      qr <- NULL
      for (j in 2:n){
        if (pr[j-1]<p[i] & p[i]<=pr[j]){qr<-j}
      }
      if (p[i] == pr[1]){qr <- 2}
    }
    else {rq<-NA}
    r0<-c(r0, qr)
  }
  return(r0)  
}  
