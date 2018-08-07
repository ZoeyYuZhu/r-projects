#2.(a)
## function 
loglh <- function (theta,s) {
  sum(log(1-cos(s-theta))) - n*log(2*pi)
}

s = c(0.52, 1.96, 2.22, 2.28, 2.28, 2.46, 2.50, 2.53, 2.54, 2.99, 3.47, 3.53, 3.70, 3.88, 3.91,4.04, 4.06, 4.82, 4.85, 5.46)
n = length(s)
theta=seq(-pi,pi, by=0.01)

vloglh = Vectorize(loglh,"theta")

## plot
plot(theta, vloglh(theta,s), main = 'The log-likelihood function plot')

#(b)
theta0 = asin(mean(s)-pi)
theta0

#(c)
## function
mle = function(s,iter,epsilon,start){      
  theta = start
  T = (-sum((-sin(s-theta)) / (1-cos(s-theta)))) / (-sum(1 / (cos(s-theta)-1)))
  for(i in 1:iter){
    theta = theta - T
    T = (-sum((-sin(s-theta)) / (1-cos(s-theta)))) / (-sum(1 / (cos(s-theta)-1)))
    if(abs(T)<epsilon){
      break
    }
  }
  list(thetahat = theta)
}

mle(s,10,0.001,theta0)

#(d)
mle(s,10,0.001,2.7)
mle(s,10,0.001,-2.7)

#(e)
g = rep(0,200)
results=list() 
theta1=seq(-pi,pi, by=2*pi/199)
for(i in 1:200) {
  g[i]= mle(s,10,0.0001,theta1[i])
    }
g
g = data.frame(g)

group1 = data.frame(g[196:200])
group2 = data.frame(theta1[181:195])
group2 = data.frame(theta1[180])
group4 = data.frame(theta1[179])
group5 = data.frame(theta1[173:178])
group6 = data.frame(theta1[171:172])
group7 = data.frame(theta1[163:170])
group8 = data.frame(theta1[118:162])
group9 = data.frame(theta1[117])
group10 = data.frame(theta1[75:116])
group11 = data.frame(theta1[56:74])
group12 = data.frame(theta1[55])
group13 = data.frame(theta1[31:54])
group14 = data.frame(theta1[30])
group15 = data.frame(theta1[26:29])
group16 = data.frame(theta1[25])
group17 = data.frame(theta1[19:24])
group18 = data.frame(theta1[14:18])
group19 = data.frame(theta1[12:13])
group20 = data.frame(theta1[1:11])

