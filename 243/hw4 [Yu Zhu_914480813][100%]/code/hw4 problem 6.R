# for a = 1, b = 1
a = 1
b = 1
x = 1
t1 = 1.5
t2 = 2
#u = runif(1,0,1)
y = rgamma(1,a,b)
f = function(x){
  x^(-3/2)*exp(-t1*x- t2/x + 2*(t1*t2)^0.5 + log((2*t2)^0.5))
}
sample = rep(0,5000)
for (i in 1:5000){
  r = min(f(y)*dgamma(x,a,b)/(f(x)*dgamma(y,a,b)),1)
  u = runif(1,0,1)
  if(u<=r){
    x = y
    sample[i] = y
  }else{
    sample[i] = x
  }
  y = rgamma(1,a,b)
}

# set the first 1000 as burn-in, and select sample every four points.
sample_f = seq(1001,5000,4)
s_f = sample[sample_f]
plot(s_f,type = "l",main = 'Dstribution with Gamma(1,1)')


mean(s_f)
# 1.14263
mean(1/s_f)
# 1.112668

#################################################
# change for a = 2, b = 2
a = 2
b = 2
plot(s_f,type = "l",main = 'Dstribution with Gamma(2,2)')
mean(s_f)
# 1.151036
mean(1/s_f)
# 1.107117

# change for a = 3, b = 3
a = 3
b = 3
plot(s_f,type = "l",main = 'Dstribution with Gamma(3,3)')
mean(s_f)
# 1.160904
mean(1/s_f)
# 1.112677

# change for a = 4, b = 4
a = 4
b = 4
plot(s_f,type = "l",main = 'Dstribution with Gamma(4,4)')
mean(s_f)
# 1.133949
mean(1/s_f)
# 1.121624
