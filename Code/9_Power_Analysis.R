library(tidyverse)
library(VGAM)


PPI_standard_error = function(n, N, sigma, rho){
  return(sigma/sqrt(n) *sqrt(1 - rho^2*N/(n+N)))
}

PPI_power = function(n, N, delta, sigma, rho, alpha = 0.05) {
  signal = delta/PPI_standard_error(n, N, sigma, rho)
  z_alpha = qnorm(1-alpha/2)
  power = pfoldnorm(z_alpha, mean = signal, lower.tail = FALSE)
  return(power)
}

find_mu = function(beta, alpha = 0.05){
  f = function(mu){pnorm(-qnorm(1-alpha/2), mu) + pnorm(qnorm(1-alpha/2), mu, lower.tail = FALSE) - 1 + beta}
  lower = 0
  upper = (qnorm(1-alpha/2) - qnorm(1-beta, lower.tail = FALSE))
  return(uniroot(f, c(lower,upper))$root)
}


classical_power_analysis = function(delta, sigma, beta, alpha = 0.05){
  signal = find_mu(beta, alpha)
  return(signal^2*sigma^2/delta^2)
}

optimal_n_N = function(n0, rho, gamma){
  if (rho <= 2*sqrt(gamma)/(1+gamma)){
    n_star = n0
    N_star = 0
  } else {
    n_star = n0*(1 -  rho^2 + sqrt(gamma*rho^2*(1-rho^2)))
    N_star = n_star*(n0 - n_star)/(n_star - (1-rho^2)*n0)
  }
  return(c(n = n_star,
           N = N_star))
}

cheapest_pair = function(delta, sigma, rho, gamma, beta, alpha = 0.05){
  n0 = classical_power_analysis(delta, sigma, beta, alpha)
  return(optimal_n_N(n0, 
                     rho, 
                     gamma))
}

most_powerful_pair = function(delta, sigma, rho, gamma, budget, cost_Y) {
  if (rho <= 2*sqrt(gamma)/(1+gamma)){
    return(c(n = budget/cost_Y, 
             N = 0))
  } else {
    n0 = budget/cost_Y/(1 - rho^2 + gamma*rho^2 + 2*sqrt(gamma*rho^2*(1 - rho^2)))
    return(optimal_n_N(n0, 
                       rho, 
                       gamma))
  }
}

######
# Plotting functions
######

power_curve = function(N, n0, rho){
  n = (-N + n0 + sqrt(N^2 + 2*N*n0 + n0^2 - 4*N*n0*rho^2))/2
  return(n)
}

add_power_curves = function(p, N, n0, rho, color = "black") {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = power_curve(N, n0, rho))
  return(p + geom_line(aes(x = N, 
                           y = n, 
                           group = n0),
                       data = df, 
                       color = color))
}

cost_curve = function(N, n0, gamma) {
  n =  (n0 - gamma*N)/(1+gamma)
}

add_cost_curves = function(p, N, n0, gamma, color = "red") {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = cost_curve(N, n0, gamma))
  return(p + geom_line(aes(x = N, y = n, group = n0),
                       data = df,
                       color = color))
}

####
# Most powerful pair visualization
####

betas = c(0.2, 0.15, 0.1, 0.05)
d = 0.2
delta = d
sigma = 1
n0_power = map_dbl(betas, \(b) classical_power_analysis(delta, sigma, b))
gamma = 0.05
rho = 0.75
optimal_pair = cheapest_pair(delta, sigma, rho, gamma, beta = betas[3])
n0_cost = optimal_pair[1]*(1 + gamma)+gamma * optimal_pair[2]

N = seq(0, 1000, 10)

p = ggplot() %>% 
  add_power_curves(N, n0_power, rho) %>% 
  add_cost_curve(N, n0_cost, gamma) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1])
p


####
# Cheapest pair visualization
####

n0_cost = c(150, 175, 200, 225)
d = 0.2
delta = d
sigma = 1

gamma = 0.05
rho = 0.75

optimal_pair = most_powerful_pair(delta, sigma, rho, gamma, n0_cost[3], 1)
beta = 1 - PPI_power(optimal_pair[1], optimal_pair[2], delta, sigma, rho)
n0_power = classical_power_analysis(delta, sigma, beta)

N = seq(0, 1000, 10)

p = ggplot() %>% 
  add_cost_curves(N, n0_cost, gamma) %>% 
  add_power_curves(N, n0_power, rho) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1])
p


