# Problem 1(b)
EM = function(theta0,iteration,epsilon,x1,x2,x3,x4){
  theta_old = theta0
  for (i in 1:iteration){
    theta_new = (x1*(0.25*theta_old/(0.5+0.25*theta_old))+x4) / (x1*(0.25*theta_old/(0.5+0.25*theta_old))+x2+x3+x4)
    if(abs(theta_new-theta_old) < epsilon){break}
    theta_old = theta_new
  }
  return(theta_new)
}
theta0 = 100
iteration = 100
epsilon = 0.0000000001
x1 = 125; x2 = 21; x3 = 20; x4 = 33
out = EM(theta0,iteration,epsilon,x1,x2,x3,x4);out

