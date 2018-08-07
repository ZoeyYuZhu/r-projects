m = 100000
# for v = 0.1
v = 0.1
i = 0
hw = rep(0,m)

for (i in 1:m){
  x = rnorm(1,1.5,v)
  if(x>1 & x<2){
    hw[i] = v*exp(-x^2/2+(x-1.5)^2/2/v^2)
  }else{hw[i] = 0}
  i = i+1
}


I = mean(hw);I
var_I = sum((hw-I)^2)/((m-1)*m);var_I
## the estimated integral is 0.1265772, variance is 0.000148637
hist(hw,main ='when v = 0.1')

#######################################

# for v = 1
v = 1
i = 0
hw = rep(0,m)

for (i in 1:m){
  x = rnorm(1,1.5,v)
  if(x>1 & x<2){
    hw[i] = v*exp(-x^2/2+(x-1.5)^2/2/v^2)
  }else{hw[i] = 0}
  i = i+1
}


I = mean(hw);I
var_I = sum((hw-I)^2)/((m-1)*m);var_I
## the estimated integral is calculated as 0.1366478, variance is 3.84406e-07
hist(hw,main ='when v = 1')

#######################################

# for v = 10
v = 10
i = 0
hw = rep(0,m)

for (i in 1:m){
  x = rnorm(1,1.5,v)
  if(x>1 & x<2){
    hw[i] = v*exp(-x^2/2+(x-1.5)^2/2/v^2)
  }else{hw[i] = 0}
  i = i+1
}


I = mean(hw);I
var_I = sum((hw-I)^2)/((m-1)*m);var_I
## the estimated integral is calculated as 0.1357925, variance is 5.201715e-07
hist(hw,main ='when v = 10')
