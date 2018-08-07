#(c)
## function
loglh <- function (theta,x) {
  -n*log(pi)-sum(log(1+(theta-x)^2))
}

## import data
x=c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,-0.23,-0.07,0.27,
    1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21,56.75)
n = length(x)
theta=seq(-5,5, by=0.01)

vloglh = Vectorize(loglh,"theta")

## plot
plot(theta, vloglh(theta,x),main = 'The log-likelihood function plot')


#(d)
## function
mlecauchy=function(x,iter,start){      
  theta = start
  for(i in 1:iter){
    theta = theta - (-2*sum((theta-x) / (1+(theta-x)^2))) / (-2*sum((1-(theta-x)^2) / ((1+(theta-x)^2)^2)))
  }
  list(thetahat = theta)
}

## import data
x=c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,-0.23,-0.07,0.27,
    1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21,56.75)

# try different starting points
mlecauchy(x,100,-11)
mlecauchy(x,100,-1)
mlecauchy(x,100,0)
mlecauchy(x,100,1.4)
mlecauchy(x,100,4.1)
mlecauchy(x,100,4.8)
mlecauchy(x,100,7)
mlecauchy(x,100,8)
mlecauchy(x,100,38)

## check the answer
optimize(function(theta) -sum(dcauchy(x, location=theta, log=TRUE)),  c(-100,100)) 


#(e)
## function
mlecauchy2=function(x,iter1,iter2,start){      
  theta = start
  n = length(x)
  for(i in 1:iter1){
    theta = theta + (-2*sum((theta-x) / (1+(theta-x)^2))) / (n/2)
  }
  for(i in 1:iter2){
    theta = theta - (-2*sum((theta-x) / (1+(theta-x)^2))) / (-2*sum((1-(theta-x)^2) / ((1+(theta-x)^2)^2)))
  }
  list(thetahat = theta)
}

## import data
x=c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,-0.23,-0.07,0.27,
    1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21,56.75)

# try different starting points
mlecauchy2(x,100,10,-11)
mlecauchy2(x,100,10,-1)
mlecauchy2(x,100,10,0)
mlecauchy2(x,100,10,1.4)
mlecauchy2(x,100,10,4.1)
mlecauchy2(x,100,10,4.8)
mlecauchy2(x,100,10,7)
mlecauchy2(x,100,10,8)
mlecauchy2(x,100,10,38)
