require(mvtnorm)
require(ggplot2)

# See: https://stat.ethz.ch/pipermail/r-help/2007-May/131726.html
autoreg_mx<-function(p, rho){
  times <- 1:p
  H <- abs(outer(times, times, "-"))
  V <- rho^H
  p <- nrow(V)
  return(V)
}

#see: https://stackoverflow.com/a/25555105
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

k = 5
p = 3
rho = 0.5
sigma_0 = autoreg_mx(p, rho)
sigma_inv = solve(sigma_0)

n = 400
t = c()
for(i in 1:10000){
  X = rmvnorm(n, sigma = k * sigma_0)
  S = cov(X)
  M = sigma_inv %*% S
  ev = eigen(M)$val
  t = c(t, n*p*log(mean(ev)/gm_mean(ev)))
}

ggplot(mpgdata, aes(x = horsepower, y = mpg)) +
  geom_point(aes(colour=cylinders), size = 2) +
  ggtitle("Fuel efficiency by Horsepower")
