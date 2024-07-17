source("Code/1_Functions.R")

###################################
# Functions to calculate statistics
###################################

ppi_standard_error = function(n, N, sigma, rho){
  return(sigma/sqrt(n) *sqrt(1 - rho^2*N/(n+N)))
}

ppi_power = function(n, N, delta, sigma, rho, alpha = 0.05) {
  signal = delta/ppi_standard_error(n, N, sigma, rho)
  z_alpha = qnorm(1-alpha/2)
  power = pnorm(z_alpha, mean = signal, lower.tail = FALSE) + pnorm(-z_alpha, mean = signal)
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
  pair = c(n = n_star, N = N_star)
  return(pair)
}

cheapest_pair = function(delta, sigma, rho, gamma, beta, alpha = 0.05){
  n0 = classical_power_analysis(delta, sigma, beta, alpha)
  pair = optimal_n_N(n0, rho, gamma)
  return(pair)
}

most_powerful_pair = function(delta, sigma, rho, gamma, budget, cost_Y) {
  if (rho <= 2*sqrt(gamma)/(1+gamma)){
    pair = c(n = budget/cost_Y, N = 0)
  } else {
    n0 = budget/cost_Y/(1 - rho^2 + gamma*rho^2 + 2*sqrt(gamma*rho^2*(1 - rho^2)))
    pair = optimal_n_N(n0, rho, gamma)
  }
  return(pair)
}

####################
# Plotting functions
####################

power_curve = function(N, n0, rho){
  n = (-N + n0 + sqrt(N^2 + 2*N*n0 + n0^2 - 4*N*n0*rho^2))/2
  return(n)
}

add_power_curves = function(p, N, n0, rho, delta, sigma, color = "black", label = FALSE) {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = power_curve(N, n0, rho),
           power = ppi_power(n, N, delta, sigma, rho))
  if (label) {
    p = p + geom_line(aes(x = N, y = n, group = n0, color = power), 
                      data = df)
  } else {
    p = p + geom_line(aes(x = N, y = n, group = n0), 
                      data = df, color = color)
  }
  
  
  return(p)
}

cost_curve = function(N, n0, gamma) {
  n = (n0 - gamma*N)/(1+gamma)
}

add_cost_curves = function(p, N, n0, gamma, color = "darkred", label = FALSE) {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = cost_curve(N, n0, gamma),
           cost = n0)
  if (label) {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0, color = cost), data = df)
  } else {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0), data = df, color = color)
  }
  
  
  return(p)
}

##################################
# Most powerful pair visualization
##################################

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

pp = ggplot() %>% 
  add_power_curves(N, n0_power, rho, delta, sigma, label=T) %>% 
  add_cost_curves(N, n0_cost, gamma) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1])
pp


#############################
# Cheapest pair visualization
#############################

n0_cost = c(150, 175, 200, 225)
d = 0.2
delta = d
sigma = 1

gamma = 0.05
rho = 0.75

optimal_pair = most_powerful_pair(delta, sigma, rho, gamma, n0_cost[3], 1)
beta = 1 - ppi_power(optimal_pair[1], optimal_pair[2], delta, sigma, rho)
n0_power = classical_power_analysis(delta, sigma, beta)

N = seq(0, 1000, 10)

cp = ggplot() %>% 
  add_cost_curves(N, n0_cost, gamma, label = T) %>% 
  add_power_curves(N, n0_power, rho, delta, sigma) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1]) 
cp



###############
# Combine plots
###############

ymax = 325
pp = pp + lims(y = c(NA, ymax)) 
cp = cp + lims(y = c(NA, ymax)) 


p = ggpubr::ggarrange(pp,cp,nrow=2,legend = "right", labels = "auto", vjust=1) 
print(p)
ggsave(filename = paste0("Figures/9_MostPowerfulAndCheapestPair.pdf"), 
       plot=p, width=7, height=6)


