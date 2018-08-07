# problem 5
# part(c)
theta = 0.5
m = 2*gamma(theta)/(2*gamma(theta)+gamma(theta+0.5))
gx = rep(0,5000)
k = 0
while(k<=5000){
  u = runif(1,0,1)
  a = rgamma(1,theta,1)
  b = rgamma(1,theta+0.5,1)
  if(u<=m){
    gx[k] = a
  }else{
    gx[k] = b
  }
  k = k+1
}

hist(gx)
plot(density(gx))

############################
# part(d)

# qx and gx functions
qx = function(x){
  sqrt(4+x) * x^(theta-1) * exp(-x)
}

gx = function(x){
  m*x^(theta-1)*exp(-x)+(1-m)*x^(theta-0.5)*exp(-x) 
}

# generate 5000 samples from fx
fx = function(theta){
  m = (2*gamma(theta))/(2*gamma(theta)+gamma(theta+0.5))
  k = 0
  alpha = 2*gamma(theta)+gamma(theta+0.5)
  fx =c()  #rep(0,5000)
  while(k<=5000){
    u = runif(1,0,1)
    a = rgamma(1,theta,1)
    b = rgamma(1,theta+0.5,1)
    if(u<=m){
      g = a
    }else{
      g = b
    }
    if(u<=(qx(g))/(alpha*gx(g))){
      fx[k] = g
      k = k+1
    }
    else{
      next
    }
  }
  return(fx)
}

# test theta as 0.5, 1, 1.5
plot(density(fx(theta = 0.5)))
lines(density(fx(theta = 1)))
lines(density(fx(theta = 1.5)))
