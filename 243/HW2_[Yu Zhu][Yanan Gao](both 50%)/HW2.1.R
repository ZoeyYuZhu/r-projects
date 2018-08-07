# Problem 1
# import data
a = c(1,2,4,9,8,3,2,1,5,7,1,2,9,3,5,3,7,2,5,1,3,4,6,6,6,1,9,6,1,4,7,7,1,6,5,9,1,3,4,5,2,1,6,5,4,2,1,2,1,3,9,1,1,2,1,3,6,8,2,5,3,5,4,7,8,3,1,2,5,2,6,1,7,9,5,1,4,9,4,2,1,1,7,8, 3,3,5,1,6,4, 9,1,8,5,2, 2,1,8,1, 5,4,3, 9,6,7)
m1 = matrix(0,15,15)
m1[lower.tri(m1,diag = F)] <-a
m2 = t(m1)
mat = m1+m2
mat
  
  
# distance function
distance = function(theta){
  d = 0
  for (i in 1:15){
    d = d+mat[theta[i],theta[i+1]]
  }
  d
}

# define initial theta
theta = 1:15
theta = c(theta, theta[1])
initial = theta

#simulated annealing
SA <- function(initial,iteration,m,tau,p){
  for (j in 1:iteration){
    for (i in 1:m){
      #substitute the location of two cities
      index = sample(1:15, 2, replace = FALSE) 
      #generate theta star
      theta.star = theta
      theta.star[index] = theta.star[c(index[2],index[1])]
      theta.star[16] = theta.star[1]

      delta = distance(theta.star) - distance(theta)
      if(delta <= 0){
        theta = theta.star
      }else{
        uniform.random.number <- runif(1)
        if(uniform.random.number < exp(-delta/tau)){
          theta = theta.star
        }else{
          theta = theta
        }
      }
    }

    tau = tau*p
    if (tau<=0.0001){
      break
    }
  }
  list(solution = theta, distance = distance(theta))
}

tau = 400
m = 100
iteration = 10000
p = 0.999

d17 = 0
d18 = 0
d19 = 0
d20 = 0
d21 = 0
sol = list()
for (i in 1:100){
  outcome = SA(initial,iteration,m,tau,p)
  print(outcome)
  sol[[i]] = outcome$solution
  if (outcome$distance == 17){
    d17 = d17+1
  }
  if (outcome$distance == 18){
    d18 = d18+1
  }
  if (outcome$distance == 19){
    d19 = d19+1
  }
  if (outcome$distance == 20){
    d20 = d20+1
  }
  if (outcome$distance == 21){
    d21 = d21+1
  }
}

# compare with different combination of p and tau
tau1 = 200
tau2 = 300
tau3 = 400
p1 = 0.9
p2 = 0.99
p3 = 0.999

m = 50
iteration = 10000

d17 = 0
d18 = 0
d19 = 0
d20 = 0
d21 = 0
sol = list()
for (i in 1:m){
  outcome = SA(initial,iteration,m,tau3,p3)
  print(outcome)
  sol[[i]] = outcome$solution
  if (outcome$distance == 17){
    d17 = d17+1
  }
  if (outcome$distance == 18){
    d18 = d18+1
  }
  if (outcome$distance == 19){
    d19 = d19+1
  }
  if (outcome$distance == 20){
    d20 = d20+1
  }
  if (outcome$distance == 21){
    d21 = d21+1
  }
}
