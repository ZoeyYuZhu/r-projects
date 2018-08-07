# Problem 3
# part(a)
n = 1500
x = runif(n,0,1)
hx = 1/(1+x)
mu = mean(hx);mu
var_mu = sum((hx-mu)^2)/((n-1)*n);var_mu
## the estimated integral is calculated as 0.6923455, variance is 1.269863e-05

# part(b)
cx = 1+x
reg = lm(hx~cx)
b_star = reg$coefficients[2]; b_star
# optimal value for b is -0.4772632
I_cv = mean(hx) - (-0.4753208)*(mean(cx)-1.5);I_cv
## the estimated integral is calculated as 0.6924141

# part(c)
rou = cor(cx,hx)
var_I_cv = var_mu*(1-rou^2);var_I_cv
## Under n = 1500, the variance under control variate is 4.030708e-07, which is smaller than 
## variance of Monte Carlo: 1.269863e-05. 

# part(d)
cx_new = x^0.5+1
rou_new =  cor(cx_new,hx)
var_I_cv_new = var_mu*(1-rou_new^2);var_I_cv_new
## variance is 5.501937e-08