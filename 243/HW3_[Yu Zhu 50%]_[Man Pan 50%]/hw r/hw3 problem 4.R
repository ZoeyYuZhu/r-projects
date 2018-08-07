Y_4g1 = runif(5000,0,1)
X_4g1 = -log(1-Y_4g1)
plot(density(X_4g1[X_4g1<5]))

###############
Y_4g2 = runif(5000,0,1)
X_4g2 = tan((pi/2)*Y_4g2)
plot(density(X_4g2[X_4g2<5]))

###############
q = function(x){
  exp(-x)/(1+x^2)
}

g1 = function(x){
  exp(-x)
}

g2 = function(x){
  2/(pi*(1+x^2))
}


#######################
# draw the sample f from g1
k = 0 
p = 0
fxg1 = rep(0,5000)
# since if u~uniform, 1-u~uniform
alpha = 1
while(k<=5000){
  Y_4g1 = runif(1,0,1)
  X_4g1 = -log(runif(1,0,1))
  if(Y_4g1 <= q(X_4g1)/(alpha*g1(X_4g1))){
    fxg1[k] = X_4g1
    k = k+1
  }
  p = p+1
}

p/5000
hist(fxg1)
plot(density(fxg1))
#############################
# draw the sample f from g2
k = 0 
p = 0
fxg2 = rep(0,5000)
alpha = pi/2
while(k<=5000){
  Y_4g2 = runif(1,0,1)
  X_4g2 = tan((pi/2)*runif(1,0,1))
  if(Y_4g2 <= q(X_4g2)/(alpha*g2(X_4g2))){
    fxg2[k] = X_4g2
    k = k+1
  }
  p = p+1
}

p/5000
plot(density(fxg2))



