j = 1
n = 200
p = 3
x = rep()
for(i in 1:n) {
  x[i] = (i-0.5)/n 
}
k = seq(0,1,1/31)[2:31]
fxi = 1.5*dnorm((x-0.35)/0.15,0,1)-dnorm((x-0.8)/0.04,0,1)


# noise level
for (j in 1:6){
  if(j==1|j==5){s.l = seq(1e-9, 1e-6, by = 1e-9)}
  if(j==2){s.l= seq(1e-11, 1e-8, by = 1e-11)}
  if(j==3){s.l = seq(1e-9, 1e-6, by = 1e-9)}
  if(j==4){s.l = seq(1e-8, 1e-5, by = 1e-8)}
  if(j==6){s.l = seq(1e-10, 1e-7, by = 1e-10)}
  
  r.cv = rep()
  r.gcv = rep()
  r.aicc = rep()
  r.risk = rep()
  den = rep()
  X.design.square = t(X.design)%*%X.design
  for (iter in 1:100){
    epsilon = rnorm(n,0,1) # sample epsilon
    y = noise(fxi,j,epsilon)
    # calculate CV, GCV under different lambda
    s.l = seq(1e-10, 1e-8, by = 1e-10)
    CV.out = rep()
    GCV.out = rep()
    RISK.out = rep()
    AICc.out = rep()
    
    
    for(lambda in s.l){
      H.lambda = X.design%*%solve(X.design.square+lambda*D)%*%t(X.design)
      y.hat = H.lambda %*% y
      CV.out = c(CV.out,CV(H.lambda))
      GCV.out = c(GCV.out, GCV(H.lambda))
      AICc.out = c(AICc.out,AICc(H.lambda))
    }
    
    sigma.cv = min(CV.out)
    sigma.gcv = min(GCV.out)
    sigma.aicc = min(AICc.out)
    
    lambda.cv = s.l[CV.out == sigma.cv]
    lambda.gcv = s.l[GCV.out == sigma.gcv]
    lambda.aicc = s.l[AICc.out == sigma.aicc]
    
    H.lambda.cv = X.design%*%solve(X.design.square+lambda.cv*D)%*%t(X.design)
    y.hat.cv = H.lambda.cv %*% y
    H.lambda.gcv = X.design%*%solve(X.design.square+lambda.gcv*D)%*%t(X.design)
    y.hat.gcv = H.lambda.gcv %*% y
    H.lambda.aicc = X.design%*%solve(X.design.square+lambda.aicc*D)%*%t(X.design)
    y.hat.aicc = H.lambda.aicc %*% y
    
    for(lambda in s.l){
      H.lambda = X.design%*%solve(X.design.square+lambda*D)%*%t(X.design)
      y.hat = H.lambda %*% y
      RISK.out = c(RISK.out,RISK(H.lambda,sigma.cv))
    }
    sigma.risk = min(RISK.out)
    lambda.risk = s.l[RISK.out == sigma.risk]
    H.lambda.risk = X.design%*%solve(X.design.square+lambda.risk*D)%*%t(X.design)
    y.hat.risk = H.lambda.risk %*% y
    
    # calculate r for different criterions
    for(lambda in s.l){
      H.lambda = X.design%*%solve(X.design.square+lambda*D)%*%t(X.design)
      y.hat = H.lambda %*% y
      den = c(den, t(fxi - y.hat) %*% (fxi- y.hat))
      
    }
    den.min = min(den)
    
    num.cv = sum((fxi - y.hat.cv)^2)
    num.gcv = t(fxi - y.hat.gcv) %*% (fxi - y.hat.gcv)
    num.aicc = t(fxi - y.hat.aicc) %*% (fxi - y.hat.aicc)
    num.risk = t(fxi - y.hat.risk) %*% (fxi - y.hat.risk)
    
    r.cv = c(r.cv, num.cv/den.min)
    r.gcv = c(r.gcv, num.gcv/den.min)
    r.aicc = c(r.aicc, num.aicc/den.min)
    r.risk = c(r.risk, num.risk/den.min)
    print(iter)
  }
  print(j)
  # plot 
  boxplot(log(r.cv),log(r.gcv),log(r.aicc),log(r.risk), ylim = c(0,1) )
}

########################################################
# y under noise level factor
noise = function(fxi,j,epsilon){
  y = fxi + (0.02+0.04*(j-1)^2)*epsilon
  return(y)
}


# design matrix
X.design = cbind(rep(1,n),x)

X.1 = rep()
for (i in 2:p){
  x.new = x^i
  X.1 = cbind(X.1, x.new)
}
X.2 = rep()

for (i in 1:30){
  x.new = (x-k[i])^p
  for (kk in 1:n){
    x.new[kk] = max(x.new[kk], 0)
  }
  X.2 = cbind(X.2, x.new)
}

X.design = cbind(X.design,X.1,X.2)

# D
D = diag(c(rep(0,p+1),rep(1,30)))

## different functions
# CV function
CV = function(H.lambda){
  (1/n) * sum(((y-y.hat)/(1-diag(H.lambda)))^2)
  #sum(((y - y.hat)/(1-diag(H.lambda)))^2)/n
}
# GCV function
GCV = function(H.lambda){
  (1/n) * (sum((y-y.hat)^2))/(1-(sum(diag(H.lambda)))/n)^2
  #sum(((y - y.hat)/(1-sum(diag(H.lambda))/n))^2)/n
}
# AICc function
AICc = function(H.lambda){
  log(sum((y-y.hat)^2)) + 2*(sum(diag(H.lambda))+1) / (n-sum(diag(H.lambda))-2)
}

# RISK function
RISK = function(H.lambda,sigma){
  t(y - y.hat)%*%(y - y.hat) + (2*(sum(diag(H.lambda)) - n))*(sigma)
}





