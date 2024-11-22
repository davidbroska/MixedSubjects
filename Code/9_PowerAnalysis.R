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

cheapest_pair = function(n0, rho, gamma){
  pair = optimal_n_N(n0, rho, gamma)
  return(pair)
}

most_powerful_pair = function(rho, gamma, budget, cost_Y) {
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

add_power_curves = function(p, N, n0, rho, color = "black", label = FALSE) {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = power_curve(N, n0, rho))
  if (label) {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0, color = factor(n0)), data = df) + 
      labs(title = "Finding the most powerful pair (n, N) for a given budget", 
           color = "Effective\nsample size") +
      scale_color_manual(
        breaks = c(300,250,200,150), 
        values = c("#99000d","#a50f15","#de2d26","#fb6a4a")
      ) 
  } else {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0), data = df, color = color)
  }
  return(p)
}

cost_curve = function(N, n0, gamma) {
  n = (n0 - gamma*N)/(1+gamma)
}

add_cost_curves = function(p, N, n0, gamma, cost_Y, color = "black", label = FALSE) {
  df = expand_grid(N = N, n0 = n0) %>% 
    mutate(n = cost_curve(N, n0, gamma),
           cost = cost_Y*n0)
  if (label) {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0, color = factor(cost)), data = df) + 
      labs(title = "Finding the cheapest pair (n, N) for a given statistical power", 
           color = "Budget") +
      scale_color_manual(
        breaks = c(225,200,175,150), 
        values = c("#005a32","#006d2c","#238b45","#74c476")
      ) 
  } else {
    p = p + 
      geom_line(aes(x = N, y = n, group = n0), data = df, color = color)
  }
  
  
  return(p)
}

##################################
# Most powerful pair visualization
##################################


n0_power = c(150, 200, 250, 300)
gamma = 0.05
rho = 0.75
cost_Y = 1
optimal_pair = cheapest_pair(n0_power[3], rho, gamma)
n0_cost = optimal_pair[1]*(1 + gamma)+gamma * optimal_pair[2]

N = seq(0, 1000, 10)

pp = ggplot() %>% 
  add_cost_curves(N, n0_cost, gamma, cost_Y) %>% 
  add_power_curves(N, n0_power, rho, label=T) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1], shape = 4, size = 2.1, stroke = 0.65) +
  theme(legend.key.width = unit(0.5, "inches"), 
        plot.margin = unit(c(0, 0, 0, 0), "inches"))  
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

optimal_pair = most_powerful_pair(rho, gamma, n0_cost[3], 1)
beta = 1 - ppi_power(optimal_pair[1], optimal_pair[2], delta, sigma, rho)
n0_power = classical_power_analysis(delta, sigma, beta)

N = seq(0, 1000, 10)

cp = ggplot() %>% 
  add_cost_curves(N, n0_cost, gamma, cost_Y, label = T) %>% 
  add_power_curves(N, n0_power, rho) +
  annotate("point", x = optimal_pair[2], y = optimal_pair[1], shape = 4, size = 2.1, stroke = 0.65) +
  theme(legend.key.width = unit(0.5, "inches"), 
        plot.margin = unit(c(0, 0, 0, 0), "inches"))  

cp



###############
# Combine plots
###############

ymax = 325
y_to_x_ratio = 1000/ymax/2
pp = pp + 
  coord_fixed(ratio = y_to_x_ratio) +
  lims(y = c(NA, ymax)) 
cp = cp + 
  lims(y = c(NA, ymax)) + 
  coord_fixed(ratio = y_to_x_ratio) 


p = ggpubr::ggarrange(cp,pp,nrow=2,legend = "right", labels = "auto", hjust=-0.5, vjust=1.5) 
print(p)
ggsave(filename = paste0("Figures/9_MostPowerfulAndCheapestPair.pdf"), 
       plot=p, width=7, height=6)


