# problem 3
Y = runif(5000,0,1)
X = -log(1-(1-exp(-2))*Y)
plot(density(X))

