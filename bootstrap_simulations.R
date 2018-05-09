#Problem 1
#Ordinary Monte-Carlo Estimate of given probability
p = 0
for(i in 1:10000){
  x = rnorm(n = 25, mean = 5, sd = 1)
  med = median(x)
  mu = mean(x)
  p = p + (abs(med - 5) <= abs(mu - 5))
}
p = p / 10000
p
#Monte-Carlo Estimate from limiting distribution
require(MASS)
x = mvrnorm(n = 10000, mu = c(0,0), Sigma = array(data = c(1, 1, 1, pi/2), dim = c(2,2)))
y = abs(x[,1]) > abs(x[,2])
p = mean(y)
p
#Bootstrap estimate of given probability
p = 0
p_vals = c()
for(j in 1:100){
  x = rnorm(n = 25, mean = 5, sd = 1)
  for(i in 1:1000){
    y = sample(x,25,replace = TRUE)
    med = median(y)
    mu = mean(y)
    p = p + (abs(med - 5) <= abs(mu-5))
  }
  p = p/1000
  p_vals = c(p_vals, p)
}
summary(p_vals)
mean(p_vals)
sd(p_vals)/mean(p_vals)

#Problem 2
p_vals = c(0.05, 0.25, 0.5, 0.75, 0.95)
for(p in p_vals){
  cat("Quantile level: ", p, "  Exact quantile: ", qchisq(p, 20), "  CLT estimate: ", 
      sqrt(20)*(sqrt(20) + qnorm(p, mean = 0, sd = sqrt(2))), "\n")
  p_bt = c()
  for(i in 1:100){
    x = rnorm(20)
    t = c()
    for(j in 1:1000){
      y = sample(x, size = 20, replace = TRUE)
      t = c(t, sum(y^2))
    }
    p_bt = c(p_bt, quantile(t, p))
  }
  print("Bootstrap Estimate: ")
  print(summary(p_bt))
  cat("Mean: ", mean(p_bt), "  CV: ", sd(p_bt)/mean(p_bt), "\n\n")
}

#Problem 5
require(goftest)
require(bootstrap)
T<-function(x){
  return(cvm.test(x, "pnorm")$statistic / length(x))
}
vjack = c()
n = 25
for(i in 1:1000){
  X = rcauchy(n)
  pseudo = jackknife(X,T)$jack.values
  vjack = c(vjack, ((n-1)^2)*var(pseudo)/n)
}
summary(vjack)
mean(vjack)
sd(vjack)/mean(vjack)