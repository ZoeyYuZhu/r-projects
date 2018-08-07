=9#3.(a)
x = c(0.02, 0.06, 0.11, 0.22, 0.56, 1.10, 0.02, 0.06, 0.11, 0.22, 0.56, 1.10)
y = c(47, 97, 123, 152, 191, 200, 76, 107, 139, 159, 201, 207)
u = 1/x
y_s = 1/y
model = lm(y_s~u)
summary(model)
model$coefficients
beta0 = 0.005107182
beta1 = 0.000247221
theta1 = 1/beta0;theta1
theta2 = theta1*beta1;theta2

#(b)
jmin=function(x,y,iter,start){      
  theta1 = start[1,1]
  theta2 = start[2,1]
  th = start
  for(i in 1:iter){
    f1 = 2*sum((y - theta1*x/(x+theta2))*(-x/(x+theta2)))
    f2 = 2*sum(y*theta1*x/(x+theta2)^2 - (theta1*x)^2/(x+theta2)^3)
    f = matrix(c(f1,f2),nrow = 2, ncol = 1)

    f11 = 2*sum((x/x+theta2)^2)
    f12 = 2*sum(x*y/(x+theta2)^2 - 2*x^2*theta1/(x+theta2)^3)
    f21 = f12
    f22 = 2*sum(-2*y*theta1*x/(x+theta2)^3 + 3*(theta1*x)^2/(x+theta2)^4)
    ff = matrix(c(f11,f12,f21,f22),nrow = 2,ncol = 2,byrow = TRUE)
   
    th = th - solve(ff) %*% f
    theta1 = th[1,1]
    theta2 = th[2,1]
  }
  list(thetahat = th)
}

origin = matrix(c(theta1,theta2),nrow = 2, ncol = 1)
jmin(x,y,10,origin)

#$thetahat
#[1,] 211.71934766
#[2,]   0.06320907

#(c)
object = function(theta){
  sum((y-theta[1]*x/(x+theta[2]))^2)
}

gradient = function(theta){
  fc = vector('numeric',length=2)
  theta1=theta[1]
  theta2=theta[2]
  fc[1] = 2*sum((y - theta1*x/(x+theta2))*(-x/(x+theta2)))
  fc[2] = 2*sum(y*theta1*x/(x+theta2)^2 - (theta1*x)^2/(x+theta2)^3)
  return(fc)
}

steep = function(x,y,epsilon,iter,step,start){
  th = start
  print(th)
  for(i in 1:iter){
      thold = th
      print(th)
      for(p in 1: 50){
        if(object(th-step*gradient(th))<object(th)){
          break
        }
        step = step/2
      }
    th = th - step*gradient(th)
    if(sum((th-thold)^2/(thold)^2) < epsilon){
      break
    }
    step=1
  }
  list(thetahat = th)
}
epsilon = 10^-10
origin = c(theta1,theta2)
steep(x,y,epsilon,10000000,1,origin)

#$thetahat
#[1] 212.13028577   0.06359665

#(d)
n = length(x)
AT <- matrix(, nrow = 2, ncol = n)
Z <- matrix(, nrow = n, ncol = 1)
for(i in 1:n){
  f1 = x[i]/(x[i]+theta2)
  f2 = -theta1*x[i]/(x[i]+theta2)^2
  f = matrix(c(f1,f2),nrow = 2, ncol = 1)
  AT[,i] <- f
  Z[i] = y[i]-theta1*x[i]/(x[i]+theta2)
}

gn = function(A,Z,epsilon,start){
  theta1 = start[1,1]
  theta2 = start[2,1]
  th = start
  T = solve(AT %*% t(AT)) %*% AT %*% Z
  while( abs(T[1,1])>epsilon[1,1] & abs(T[2,1])>epsilon[2,1]){
    th = th + T
    theta1 = th[1,1]
    theta2 = th[2,1]
    for(i in 1:length(x)){
      f1 = x[i]/(x[i]+theta2)
      f2 = -theta1*x[i]/(x[i]+theta2)^2
      f = matrix(c(f1,f2),nrow = 2, ncol = 1)
      AT[,i] <- f
      Z[i] = y[i]-theta1*x[i]/(x[i]+theta2)
    }
    T = solve(AT %*% t(AT)) %*% AT %*% Z
  }
  list(thetahat = th)
}

epsilon = matrix(0.0001,0.0001,nrow = 2, ncol = 1)
origin = matrix(c(theta1,theta2),nrow = 2, ncol = 1)
gn(x,y,epsilon,origin)

#$thetahat
#[1,] 212.6736430
#[2,] 0.0641055
