# See: https://stat.ethz.ch/pipermail/r-help/2007-May/131726.html
autoreg_mx<-function(p, rho){
  times <- 1:p
  H <- abs(outer(times, times, "-"))
  V <- rho^H
  p <- nrow(V)
  return(V)
}

I<-function(p){
  return(diag(rep(1,p)))
}

compundsymm_mx<-function(p, rho){
  one_p = rep(1,p)
  J = one_p %*% t(one_p)
  return(rho*J + (1-rho)*I(p))
}