# Load functions
source("Code/1_Functions.R")

cost = function(nprompt, ntok_in, price1k_in, ntok_out, price1k_out, verbose=T){
  
  p_in  = price1k_in / 1000
  p_out = price1k_out / 1000

  dollars = nprompt*(ntok_in * p_in + ntok_out * p_out)

  return(dollars)
}

# pricing: https://openai.com/api/pricing/

# length of scenario description
ntok_in  = 390  
# output
ntok_out = 6    


##################
# Costs per sample
##################

# survey responses from 2097 users
nprompt = 22315 

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c_gpt35 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c_gpt4t = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c_gpt4o = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.005, price1k_out = 0.015)

writeLines(paste0("gpt-3.5-turbo-0125: ",round(c_gpt35,2),"$ for ",nprompt," API calls."))
writeLines(paste0("gpt-4-turbo: ",round(c_gpt4t,2),"$ for ",nprompt," API calls."))
writeLines(paste0("gpt-4o: ",round(c_gpt4o,2),"$ for ",nprompt," API calls."))
writeLines(paste0("Total cost: ", round(sum(c(c_gpt35,c_gpt4t,c_gpt4o)),2),"$."))

# gpt-3.5-turbo-0125: 4.55$ for API 22315 calls.
# gpt-4-turbo: 91.05$ for API 22315 calls.
# gpt-4o: 45.52$ for API 22315 calls.
# Total cost: 141.12$.


##################
# Costs per prompt
##################

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c1_gpt35 = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c1_gpt4t = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c1_gpt4o = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.005, price1k_out = 0.015)

avg_c1 = mean(c1_gpt35,c1_gpt4t,c1_gpt4o)

writeLines(paste0("gpt-3.5-turbo-0125: ",c1_gpt35,"$ for one API call."))
writeLines(paste0("gpt-4-turbo: ",c1_gpt4t,"$ for one API call."))
writeLines(paste0("gpt-4o: ",c1_gpt4o,"$ for one API call."))
writeLines(paste0("Average cost: ",avg_c1,"$ for one API call."))

# gpt-3.5-turbo-0125: 0.000204$ for one API call.
# gpt-4-turbo: 0.00408$ for one API call.
# gpt-4o: 0.00204$ for one API call.






###########################
# Cost saving Moral Machine
###########################

# Cost of a 12 minute survey response with California minimum wage $16.00/hour 
c1_human = 12 * 16/60                # $3.20    
c1_llm   = round(13*c1_gpt35,3)     # $0.002652 to $0.003

# Function to calculate the percent saved when complementing human with silicon subjects
pcost = function(.rho, .cX, .cf, .cY, .verbose=F){
  
  # cost of silicon sampling as a share of sampling human responses
  gamma = (.cX + .cf) / .cY
  
  
  # check is rho sufficiently large
  minimum_rho = (2*sqrt(gamma)) / (1+gamma)
  is_sufficient = .rho > minimum_rho
  
  # print warning if not
  if(any(!is_sufficient)) print("Rho is not sufficiently large for some cases.")
  
  pcost = .rho^2 - gamma * .rho^2 - 2*sqrt(gamma * .rho^2 * (1-.rho^2))
  
  if(.verbose){
    print(paste0("gamma: ", gamma))
    print(paste0("minimum rho: ", round(minimum_rho,3)))
  }
  
  return(pcost)
}

# % saved
psaving = pcost(.rho=0.35, .cX=0, .cf=c1_llm, .cY=c1_human, .verbose = T)
psaving

# 165.63$ saved in a n=500 study
500*c1_human * psaving


############################################
# Percent of recruiting human subjects saved
############################################


dd = expand.grid(rho = seq(0,1,by=0.025),
                 cX = 0,
                 cf = c(0.0001, 0.001, 0.01, 0.1),
                 cY = 1) %>% 
  mutate(pcost = 100 * pcost(.cX = cX, .cf = cf, .cY = cY, .rho = rho), 
         gamma = (cX + cf) / cY, 
         gamma_formatted = paste0(100*gamma, "%"),
         is_sufficient = ifelse(rho > (2*sqrt(gamma)) / (1+gamma), 1, 0), 
         pcost = ifelse(!is_sufficient, NA, pcost)
  ) 

colors = tribble(
  ~Variable,   ~Code,        ~Label,   
  "0.01%",     "#57606CFF",  "0.01%", 
  "0.1%",      "#1170AAFF",  "0.1%",  
  "1%",        "#5FA2CEFF",  "1%",     
  "10%",       "#A3CCE9FF",  "10%",    
)

title = expression(paste("Cost of predicting a response as a\nshare of recruiting a human subject (", gamma, ")"))

ggplot(dd, aes(rho, pcost, color = gamma_formatted, linetype = gamma_formatted)) + 
  geom_line() +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = colors$Code, breaks = colors$Variable, labels= colors$Label) +
  labs(x = bquote(tilde(rho)), 
       y = "% of cost of recruiting human subjects saved") +
  guides(color = guide_legend(title = title), 
         linetype = guide_legend(title = title)) +
  theme(panel.grid.major = element_line(size = 0.2), panel.grid.minor = element_line(size = 0.1))
ggsave(filename = "Figures/3_PercHumanSubjectsSaved.pdf", width=7, height=6)



####################
# Most powerful pair
####################

SE_ppi = function(.n, .N, .rho, .sigma_classic){
    return(.sigma_classic/sqrt(.n) * sqrt(1-.rho^2 * (.N / (.N+.n))))
}

power_ppi = function(.delta, .n, .N, .rho, .sigma_classic = 1, .alpha=0.05){
  

  # estimated standard error of PPI parameter
  se_ppi = SE_ppi(.n, .N, .rho, .sigma_classic)
  
  q1 = qnorm(.alpha/2, lower.tail = F) - .delta * (1/se_ppi)
  p1 = pnorm(q1, mean=0, sd=1, lower.tail=F)
  
  q2 = - qnorm(.alpha/2, lower.tail = F) - .delta * (1/se_ppi)
  p2 = pnorm(q2, mean=0, sd=1, lower.tail=T)

  
  ppi_power = p1 + p2
  
  return(ppi_power)
}

effect_h1 = 0.1
effect_h0 = 0
delta = effect_h1 - effect_h0


dd_mpp = expand.grid(
  n = c(10, seq(50,1000,50)),
  N = c(10,50, seq(100,5000,100)),
  rho = c(0.4, 0.8),
  budget = 500, 
  gamma = c(0.01),
  delta) %>% 
  mutate(
    cost_tradeoff = (budget - gamma * N)/(1+gamma),
    se_ppi = SE_ppi(.n=n, .N=N, .rho=rho, 1),
    power = power_ppi(.delta=delta,.n=n,.N=N,.rho=rho))

dd_mpp %>% 
  ggplot() +
  geom_contour(aes(x = N, y = n, z = power),
                      breaks = c(0, 0.8, 0.9, 0.95, 1)) +
  geom_line(aes(N, cost_tradeoff)) +
  facet_wrap(~rho) 


###############################
# Cheapest pair for given power
###############################


dd_cp = expand.grid(
  n = c(10, seq(50,1000,50)),
  N = c(10,50, seq(100,5000,100)),
  power = c(0.8, 0.9, 0.95),
  rho = c(0.4, 0.8),
  budget = 500, 
  gamma = c(0.01),
  delta) %>% 
  mutate(
    cost_tradeoff = (budget - gamma * N)/(1+gamma),
    se_ppi = SE_ppi(.n=n, .N=N, .rho=rho, 1))

dd_mpp %>% 
  ggplot() +
  geom_contour_filled(aes(x = N, y = n, z = power),
                      breaks = c(0, 0.8, 0.9, 0.95, 1)) +
  geom_line(aes(N, cost_tradeoff)) +
  facet_wrap(~rho) 


#####################
# indifference curves
#####################


power_curve = function(N, n0, rho){
  n = (-N + n0 + sqrt(N^2 + 2*N*n0 + n0^2 - 4*N*n0*rho^2))/2
  return(n)
}


n_star = function(n0, rho, gamma) {
  return( n0*(1 - rho^2 + sqrt(gamma * rho^2 * (1 - rho^2))) )
}

N_star = function(n0, rho, gamma) {
  n_opt = n_star(n0, rho, gamma)
  N = n_opt * (n0 - n_opt)/(n_opt - n0*(1 - rho^2))
  return(N)
}

c_star = function(n0, rho, gamma) {
  cost = n0*(1 - rho^2 + gamma*rho^2 + 2*sqrt(gamma*rho^2*(1-rho^2)))
  return(cost)
}


cost_curve = function(N, cost, gamma) {
  n =  (cost - gamma*N)/(1+gamma)
}



rhos = c(0.5, 0.75)
gammas = c(0.04, 0.02)
n0s = c(100, 200)
Ns =  seq(0,1000, length.out = 100)


df_cost = expand_grid(
  n0 = n0s,
  gamma = gammas,
  rho = rhos,
  N = Ns
) %>% 
  mutate(
    n_opt = n_star(n0, rho, gamma),
    N_opt = N_star(n0, rho, gamma),
    n_cost = cost_curve(N, c_star(n0, rho, gamma), gamma),
    n_power = power_curve(N, n0, rho)) 



ggplot(df_cost) +
  geom_line(aes(x = N, y = n_power, group = n0,color="power")) +
  geom_line(aes(x = N, y = n_cost, group = n0,color="cost")) +
  geom_point(aes(x = N_opt, y = n_opt, group = n0), size=0.75) +
  facet_grid(
    rows = vars(gamma),
    cols = vars(rho),
    labeller = label_bquote(
      rows = gamma==.(gamma), 
      cols = tilde(rho)==.(rho)
    )
  ) + labs(
    y = "n",
    title = "Indifference curves for a pair of n and N that gives the same statistical power",
    color = "Curves"
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("power"="black", "cost"="darkred"), 
    labels = c("power"="Statistical power", "cost"="Cost")) 
ggsave(filename = "Figures/3_IndifferenceMostPowerfulPair.pdf", width=7, height=6)


####
# Lowest cost given power
###


n0 <- 200 # power level determined by n0
gamma <- 0.05
rho <- 0.75
Ns <- seq(0, 1000, length.out = 100)

df_lowest_cost <- tibble(
  N = Ns,
  n_power = power_curve(N, n0, rho),
  n_cost_opt = cost_curve(N, c_star(n0, rho, gamma), gamma),
  n_cost_bad = cost_curve(N, n0*0.8, gamma)
)

ggplot(df_lowest_cost) +
  geom_line(aes(x = N, y = n_power), color = "black") +
  geom_line(aes(x = N, y = n_cost_opt ), color = "darkred") +
  geom_line(aes(x = N, y = n_cost_bad), color = "darkred") +
  labs(title = "Same level of power, higher cost")



####
# Highest power given budget
###


n0 <- 200 # power level determined by n0
gamma <- 0.05
rho <- 0.75
Ns <- seq(0, 1000, length.out = 100)

df_lowest_cost <- tibble(
  N = Ns,
  n_power_opt = power_curve(N, n0, rho),
  n_cost = cost_curve(N, c_star(n0, rho, gamma), gamma),
  n_power_bad = power_curve(N, n0*0.8, rho)
)

ggplot(df_lowest_cost) +
  geom_line(aes(x = N, y = n_power_opt), color = "black") +
  geom_line(aes(x = N, y = n_power_bad ), color = "black") +
  geom_line(aes(x = N, y = n_cost), color = "darkred") +
  labs(title = "Same cost, lower power")
