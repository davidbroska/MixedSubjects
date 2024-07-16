library(tidyverse)
library(VGAM)


PPI_standard_error <- function(n, N, sigma, rho){
  return(sigma/sqrt(n) *sqrt(1 - rho^2*N/(n+N)))
}

PPI_power <- function(n, N, delta, sigma, rho, alpha = 0.05) {
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

optimal_n_N <- function(n0, rho, gamma){
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
    n0 = B/cost_Y/(1 - rho^2 + gamma*rho^2 + 2*sqrt(gamma*rho^2*(1 - rho^2)))
    return(optimal_n_N(n0, 
                       rho, 
                       gamma))
  }
}
