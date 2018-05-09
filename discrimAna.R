require(MASS)
require(mvtnorm)
require(mixAK)
require(ggplot2)

n_train = 1000
n_test = 10000

######sit1: lda and qda both work.#####
mu1 = c(0,0)
mu2 = c(0,1.5)
mu3 = c(0,-1.5)
sigma1 = cbind(c(1,0),c(0,1))/5
sigma2 = sigma1
sigma3 = sigma1

#####sit2: lda fails, qda works#####
mu1 = c(0,0)
mu2 = mu1
mu3 = c(1.5,0)
sigma1 = cbind(c(1,0.8),c(0.8,1))/5
sigma2 = cbind(c(1,-0.8),c(-0.8,1))/5
sigma3 = cbind(c(1,0),c(0,1))/5

#####sit3:lda, qda both fail, horribly (samples generated here)#####
mu1 = rbind(t(c(1,1)), t(c(3,1)), t(c(2,3)))
sigma1 = list(cbind(c(1,0.8),c(0.8,1))/5, cbind(c(1,-0.8),c(-0.8,1))/5, cbind(c(0.2,0),c(0,1))/5)

mu2 = rbind(t(c(1,3)), t(c(3,3)), t(c(2,1)))
sigma2 = list(cbind(c(1,-0.8),c(-0.8,1))/5, cbind(c(1,0.8),c(0.8,1))/5,  cbind(c(0.2,0),c(0,1))/5)


mu3 = c(2,2.5)
sigma3 = cbind(c(1,0.8),c(0.8,1))/5

x1 = rMVNmixture(n_train, weight = c(0.3,0.3,0.4), mean = mu1, Sigma = sigma1)
x2 = rMVNmixture(n_train, weight = c(0.3,0.3,0.4), mean = mu2, Sigma = sigma2)
x3 = rmvnorm(n_train, mean = mu3, sigma = sigma3)

y1 = rMVNmixture(n_test, weight = c(0.3,0.3,0.4), mean = mu1, Sigma = sigma1)
y2 = rMVNmixture(n_test, weight = c(0.3,0.3,0.4), mean = mu2, Sigma = sigma2)
y3 = rmvnorm(n_test, mean = mu3, sigma = sigma3)

x = rbind(x1,x2,x3)
g1 = as.factor(c(rep(1,n_train), rep(2,n_train), rep(3,n_train)))
y = rbind(y1,y2,y3)
g2 = as.factor(c(rep(1,n_test), rep(2,n_test), rep(3,n_test)))

p1<-function(x){
  return(dMVNmixture(x, weight = c(0.3,0.3,0.4), mean = mu1, Sigma = sigma1, log = F))
}
p2<-function(x){
  return(dMVNmixture(x, weight = c(0.3,0.3,0.4), mean = mu2, Sigma = sigma2, log = F))
}
p3<-function(x){
  return(dmvnorm(x, mean = mu3, sigma = sigma3, log = F))
}
######generate normal samples and probability functions######
x1 = rmvnorm(n_train, mean = mu1, sigma = sigma1)
x2 = rmvnorm(n_train, mean = mu2, sigma = sigma2)
x3 = rmvnorm(n_train, mean = mu3, sigma = sigma3)

y1 = rmvnorm(n_test, mean = mu1, sigma = sigma1)
y2 = rmvnorm(n_test, mean = mu2, sigma = sigma2)
y3 = rmvnorm(n_test, mean = mu3, sigma = sigma3)

x = rbind(x1,x2,x3)
g1 = as.factor(c(rep(1,n_train), rep(2,n_train), rep(3,n_train)))
y = rbind(y1,y2,y3)
g2 = as.factor(c(rep(1,n_test), rep(2,n_test), rep(3,n_test)))

p1<-function(x){
  return(dmvnorm(x, mean = mu1, sigma = sigma1, log = F))
}
p2<-function(x){
  return(dmvnorm(x, mean = mu2, sigma = sigma2, log = F))
}
p3<-function(x){
  return(dmvnorm(x, mean = mu3, sigma = sigma3, log = F))
}

######LDA#####
cl = lda(x = x, grouping = g1)
pred_cl = predict(cl, x)$class
train_err = sum(g1!=pred_cl)/(3*n_train)
pred_cl = predict(cl, y)$class
test_err = sum(g2!=pred_cl)/(3*n_test)

train_err
test_err

######QDA#####
cl = qda(x = x, grouping = g1)
pred_cl = predict(cl, x)$class
train_err = sum(g1!=pred_cl)/(3*n_train)
pred_cl = predict(cl, y)$class
test_err = sum(g2!=pred_cl)/(3*n_test)

train_err
test_err

#####Optimal Error#####
P = cbind(p1(y),p2(y),p3(y))
pred_cl = max.col(P)
opt_err = sum(g2!=pred_cl)/(3*n_test)

opt_err

#####Plot#####
cl_data = data.frame(x = x, Population = g1)
ggplot(cl_data, aes(x = x.1, y = x.2)) +
  geom_point(aes(colour=Population), size = 1) +
  ggtitle("Data by class")


#####Real dataset#####
banknote = read.csv("D:\\data_banknote_authentication.txt", header = F)
names(banknote) = c('var.img','skew.img','cur.img','entr.img','class')
banknote$class = as.factor(banknote$class)
head(banknote)
# var.img skew.img cur.img entr.img class
# 1 3.62160   8.6661 -2.8073 -0.44699     0
# 2 4.54590   8.1674 -2.4586 -1.46210     0
# 3 3.86600  -2.6383  1.9242  0.10645     0
# 4 3.45660   9.5228 -4.0112 -3.59440     0
# 5 0.32924  -4.4552  4.5718 -0.98880     0
# 6 4.36840   9.6718 -3.9606 -3.16250     0
nrow(banknote)
# [1] 1372
banknote = banknote[sample(1:1372),]
head(banknote)
# var.img skew.img  cur.img entr.img class
# 6     4.36840   9.6718 -3.96060 -3.16250     0
# 1244 -5.06760  -5.1877 10.42660 -0.86725     1
# 195  -2.34100  12.3784  0.70403 -7.58360     0
# 1109  1.45010   3.6067 -4.05570 -1.59660     1
# 135  -1.04010   9.3987  0.85998 -5.33360     0
# 1328 -0.24037  -1.7837  2.13500  1.24180     1
training = banknote[1:300,]
test = banknote[301:1372,]


cl = lda(class~.,data = training)
g1 = predict(cl, training[,1:4])$class
train_err = sum(training$class != g1)/300
g2 = predict(cl, test[,1:4])$class
test_err = sum(test$class != g2)/1072

train_err
# [1] 0.02
test_err
# [1] 0.02425373

cl = qda(class~.,data = training)
g1 = predict(cl, training[,1:4])$class
train_err = sum(training$class != g1)/300
g2 = predict(cl, test[,1:4])$class
test_err = sum(test$class != g2)/1072
train_err
# [1] 0.01666667
test_err
# [1] 0.01679104
