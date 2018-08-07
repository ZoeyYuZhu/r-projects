# problem 4
lambda=c(0.5,1,10)
outcome = list(length(lambda))
n = 500
alpha = 0.05

# define BCa function
BCa = function(x,la.hat,alpha,B=200){
  la.hatb = sapply(1:B,function(i){
    xb = sample(x,length(x),replace=TRUE)
    length(xb)/sum(xb)
  })
  a = sum((mean(la.hatb)-la.hatb)^3)/(6*(sum((mean(la.hatb)-la.hatb)^2))^1.5)
  z = qnorm(sum(la.hatb<la.hat)/B)
  a1 = pnorm(z+(z+qnorm(alpha/2))/(1-a*(z+qnorm(alpha/2))))
  a2 = pnorm(z+(z+qnorm(1-alpha/2))/(1-a*(z+qnorm(1-alpha/2))))
  quantile(la.hatb,c(a1,a2))
}

l = numeric(length(lambda))
la.hat = numeric(length(lambda))
len.lambda = length(lambda)

for(i in 1:len.lambda){
  la = lambda[i]
  X = rexp(n,lambda[i])
  la.hat[i] = n/sum(X)
  
  CIe = c(la.hat[i] * qgamma(alpha/2,n,1) / n, la.hat[i] * qgamma(1-alpha/2,n,1) / n)
  CIa = c(la.hat[i] * exp(qnorm(alpha/2)/n^0.5), la.hat[i] * exp(qnorm(1-alpha/2)/n^0.5))
  CIb = BCa(X,la.hat[i],alpha)
  
  outcome[[i]] = rbind(CIa,CIe,CIb)
  colnames(outcome[[i]])=NULL
  l[i] = which.min(outcome[[i]][,2] - outcome[[i]][,1])
}
