last = c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
gpa = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 3.96)
X = rbind(last,gpa)
#X = data.frame(X)
X = as.matrix(X,dimnames = NULL)
n = length(last)
#(a)
cor(last,gpa)

#(b)
#thetahat.i = data.frame()
thetahat.sum = matrix(0,nrow=2,ncol = n-1)
for(i in 1:n){
  thetahat.i = c(mean(X[1,-i]),mean(X[2,-i]))
  thetahat.sum = thetahat.sum + thetahat.i
}
thetahat.dot = c(mean(thetahat.sum[1,]),mean(thetahat.sum[2,]))
thetahat.dot 

for(i in 1:n){
  thetahat.i = X[,-i]
  (thetahat.i - thetahat.dot)
}