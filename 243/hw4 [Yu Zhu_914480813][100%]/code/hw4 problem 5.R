# part(a)
n = 100
p = 0.3
lambda = 2
rsam = rbinom(n,1,p)
x = rpois(n,lambda = lambda*rsam);x

#[1] 0 2 0 0 0 3 0 2 0 0 4 0 0 0 0 0 0 4 0 0 0 1 2 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0
#[41] 0 0 0 1 0 0 0 1 0 0 0 4 0 0 0 2 0 0 0 0 1 3 0 3 0 1 0 0 0 0 3 5 5 0 0 1 1 0 0 0
#[81] 0 1 0 0 0 0 0 2 0 0 1 0 0 0 0 0 0 0 0 2

#############################################
# part(c)
# set a = b = 1
a = 1
b = 1
r = rbinom(n,1,runif(1))
Lambda = rep(0,10000)
P = rep(0,10000)

for(i in 1:10000) {
  lambda = rgamma(1,a+sum(x),b+sum(r))
  Lambda[i] = lambda
  p = rbeta(1,1+sum(r),n+1-sum(r))
  P[i] = p
  prob = (p*exp(-lambda)) / (p*exp(-lambda) + (1 - p)*(x==0))
  r = rbinom(n,1,prob)
}

# set the first 1000 samples as burn-in
sample = seq(1001,10000)
lam_out = Lambda[sample]
p_out = P[sample]

# plots and confidence intervals for lambda and p
plot(lam_out,type = 'l',main = 'lambda when a = b = 1')
plot(p_out,type = 'l',main = 'p when a = b = 1')
quantile(lam_out,c(0.025,0.975))
#   2.5%      97.5% 
# 1.345102  2.521524 
## so 2 is included in this confidence interval
quantile(p_out,c(0.025,0.975))
#  2.5%      97.5% 
# 0.2067333  0.4171872 
## so 0.3 is included in this confidence interval
##################################
# set a = b = 2
a = 2
b = 2
plot(lam_out,type = 'l',main = 'lambda when a = b = 2')
plot(p_out,type = 'l',main = 'p when a = b = 2')
quantile(lam_out,c(0.025,0.975))
#  2.5%      97.5% 
# 1.307687   2.462401 
## so 2 is included in this confidence interval
quantile(p_out,c(0.025,0.975))
#  2.5%      97.5% 
# 0.2066182  0.4230448 
## so 0.3 is included in this confidence interval
####################################
# set a = b = 5
a = 5
b = 5
plot(lam_out,type = 'l',main = 'lambda when a = b = 5')
plot(p_out,type = 'l',main = 'p when a = b = 5')
quantile(lam_out,c(0.025,0.975))
#  2.5%      97.5% 
# 1.259505 2.297989 
## so 2 is included in this confidence interval
quantile(p_out,c(0.025,0.975))
#  2.5%      97.5% 
# 0.2119599 0.4322999 
## so 0.3 is included in this confidence interval

#########################################
x_l = c(1.345102,2.521524,1.307687,2.462401,1.259505,2.297989)
X_l = matrix(x,nrow = 3,byrow = T)
y = c(1,1,2,2,5,5)
Y = matrix(y,nrow = 3,byrow = T)
plot(Y,X_l, main  = 'the 95% confidence intervals of lambda')

x_p = c(0.2067333,0.4171872 ,0.2066182,0.4230448,0.2119599,0.4322999)
X_p = matrix(x_p,nrow = 3,byrow = T)
plot(Y,X_p, main  = 'the 95% confidence intervals of p')

## From the plots, we can find that the larger both a and b are(since we set a and b are the same),
## the relative shorter the confidence interval of lambda is, the same as p.