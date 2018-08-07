# Problem 2
# PART ONE: original paramters

# import data
truefunction<-function(x){
  t <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
  h <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
  temp <- 0
  for(i in 1:11) {
    temp <- temp + h[i]/2 * (1 + sign(x - t[i]))
  }
  return(temp)
}
n<-512
x<-(0:(n-1))/n
f<-truefunction(x)
set.seed(0401)
y<-f+rnorm(f)/3
plot(x,y)
lines(x,f)

# inital breakpoint
ind=c(1,1+which(diff(f)!=0))
x[ind]


# fitted function
f_hat <- function(chromosome,x,y){
  meany = c()
  fitted = c()
  nk = c()
  breakpoint.index = c(1, which(chromosome==1), 513) #return the index of 1 in chrome j, with the 1st and bBth point as "breakpoint".
  B = length(breakpoint.index)-1 #number of piesces
  for (k in 1:B){
    meany[k] = mean(y[breakpoint.index[k]:(breakpoint.index[k+1]-1)]) #record the sum y of each piece
    nk[k] = breakpoint.index[k+1]-breakpoint.index[k] #record the nj of each piece
  }
  fitted = rep(meany,nk)
  list(B=B, nk=nk,fitted=fitted)
}

# MDL function
MDL <- function(chromosome,x,y){
  fhat = f_hat(chromosome,x,y)
  B = fhat$B
  nk = fhat$nk
  fitted = fhat$fitted
  mdl = B*log(n) + (1/2)*sum(log((nk))) + (n/2)*log((1/n)*sum((y-fitted)^2))
  return(mdl)
}

# AIC function
AIC = function(chromosome,x,y){
  fhat = f_hat(chromosome,x,y)
  B = fhat$B
  fitted = fhat$fitted
  n*log(sum((y-fitted)^2)/n) + log(n)*2*B
}


# Genetic Alogrithm
GA = function(iteration,pop,s,Pc,Pcross,Nsame,Obj_f,x,y){
  t = 0
  FF = rep(NA, iteration+1) # recording the f values, including the initial f value
  diff = rep(NA, iteration) # recording the difference f values between two generations
  #population = S chromosomes
  obj = rep(0,s)
  for(j in 1:s){
    obj[j] = Obj_f(pop[j,],x,y)
  } 
  df.temp = cbind(obj, pop)
  df.sort = df.temp[order(df.temp[,1]),]
  population_ini = df.sort[,-1]

  best_c = population_ini[1,]
  best_c = as.vector(best_c)
  rank = s:1 

  prob = rank/sum(rank)
  FF[1] = Obj_f(best_c,x,y)
  population = pop
  
  for(i in 1:iteration){
    # produce child chromosome
    child = matrix(NA, nrow = s, ncol = n)
    child[1,] = best_c

    for (j in 2:s){
      if(runif(1)<Pcross){
        # do crossover 
        index = 1:s
        index_pc = sample(index,2,prob = prob)

        parent1 = population[index_pc[1],]
        parent2 = population[index_pc[2],]
        temp = rbinom(n,1,0.5)
        child[j,] = temp*parent1 + (1-temp)*parent2
        

      } else {
        # otherwise do mutation
        index_pm = sample(index,1,prob = prob)
        parent = population[index_pm,]
        temp = c(0,rbinom(n-1,1,Pc)) 
        child[j,] = temp*(1-parent) + (1-temp)*parent
      }
    }
    
    # update
    for(j in 1:s){
      obj[j] = Obj_f(child[j,],x,y)
    }

    df.temp = cbind(obj, child)
    df.sort = df.temp[order(df.temp[,1]),] 
    population_new = df.sort[,-1]
    best_c = population_new[1,]

    prob = rank/sum(rank)
    FF[i+1] = Obj_f(best_c,x,y)
    population = population_new
    print(FF[i+1])
    
    # stopping criterion
    if(i==1) diff[i] = FF[i+1]-FF[i]
    if(i>1){
      diff[i] = FF[i+1]-FF[i]
      if(diff[i]==0 & diff[i-1]==0) {
        t = t+1
      } else {t = 0}
    }
    if (t == Nsame) {
      break
    }
  }
  list(best = best_c, dif = diff, result = FF)
}


# initial values
s = 300 
Pcross = 0.9
Pc = 0.05 
Nsame = 20
iteration = 500
pop = cbind(rep(0,s), matrix(rbinom(s*(n-2), 1, 0.5), nrow = s, ncol = n-2),rep(0,s))
outc = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)


# Optimal values
## AIC
outc = GA(iteration,pop,s,Pc,Pcross,Nsame,AIC,x,y)
AIC(outc$best)

## MDL
outc = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)
MDL(outc$best)


# plot
## AIC
### fit plot
best_chromo = outc$best
breakpoint_aic = c(which(best_chromo==1))
f_best = f_hat(best_chromo)$fitted
plot(x,y)
lines(x,f_best)
### breakpoints position
x[breakpoint_aic] 
### iteration plot
plot(outc$result[2:140], main = "AIC function values through iterations", xlab = "Generations", ylab = "Function values")

## MDL
### fit plot
best_chromo = outc$best
breakpoint_mdl = c(which(best_chromo==1))
f_best = f_hat(best_chromo)$fitted
plot(x,y)
lines(x,f_best)
### breakpoints position
x[breakpoint_mdl] 
### iteration plot
plot(outc$result[2:140], main = "MDL function values through iterations", xlab = "Generations", ylab = "Function values")

#########################################################

# PART TWO: change parameters
## change s, keep other fixed
### s = 50/100/200/800
s = 800
Pcross = 0.9
Pc = 0.05 
Nsame = 20
iteration = 500
pop = cbind(rep(0,s), matrix(rbinom(s*(n-2), 1, 0.5), nrow = s, ncol = n-2),rep(0,s))
outc1 = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)
outc2 = GA(iteration,pop,s,Pc,Pcross,Nsame,AIC,x,y)

## change Pcross, keep other fixed
### Pcross = 0.8/0.9/0.99
s = 300 
Pcross = 0.99
Pc = 0.05 
Nsame = 20
iteration = 500
pop = cbind(rep(0,s), matrix(rbinom(s*(n-2), 1, 0.5), nrow = s, ncol = n-2),rep(0,s))
outc1 = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)
outc2 = GA(iteration,pop,s,Pc,Pcross,Nsame,AIC,x,y)

## change Pc, keep other fixed
### Pc = 0.1/0.15/0.03
s = 300 
Pcross = 0.8
Pc = 0.03 
Nsame = 20
iteration = 500
pop = cbind(rep(0,s), matrix(rbinom(s*(n-2), 1, 0.5), nrow = s, ncol = n-2),rep(0,s))
outc1 = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)
outc2 = GA(iteration,pop,s,Pc,Pcross,Nsame,AIC,x,y)

## change Nsame, keep other fixed
### Nsame = 10/30/50
s = 300 
Pcross = 0.8
Pc = 0.05 
Nsame = 50
iteration = 500
pop = cbind(rep(0,s), matrix(rbinom(s*(n-2), 1, 0.5), nrow = s, ncol = n-2),rep(0,s))
outc1 = GA(iteration,pop,s,Pc,Pcross,Nsame,MDL,x,y)
outc2 = GA(iteration,pop,s,Pc,Pcross,Nsame,AIC,x,y)