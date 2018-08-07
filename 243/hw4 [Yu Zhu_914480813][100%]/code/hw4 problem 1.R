# Problem 1
n = 1000000
# part(a)
x_a = runif(n,0,1)
hx_a = x_a^2
mu_a = mean(hx_a);mu_a
varmu_a = sum((hx_a-mu_a)^2)/((n-1)*n);varmu_a
## the estimated integral is calculated as 0.3333605, variance is 8.885079e-08

# part(b)
x_b = runif(n,-2,2)
y_b = runif(n,0,1)
hxy_b = 4*x_b^2*cos(x_b*y_b)
mu_b = mean(hxy_b);mu_b
varmu_b = sum((hxy_b-mu_b)^2)/((n-1)*n);varmu_b
## the estimated integral is calculated as 3.485711, variance is 1.274877e-05

# part(c)
x_c = rexp(n,0.25)
hx_c = 3*x_c^4*exp(-0.25*(x_c^3-x_c))
mu_c = mean(hx_c);mu_c
varmu_c = sum((hx_c-mu_c)^2)/((n-1)*n);varmu_c
## the estimated integral is calculated as 2.276883, variance is 1.348142e-05

