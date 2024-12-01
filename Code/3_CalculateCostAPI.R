# Load functions
source("Code/RFunctions.R")


##################################################################################
# Plot effective sample size against N for different values of the PPI correlation
##################################################################################

# Define function that calculates the effective sample size
n0 = function(rho, n, k) {
  # Define k as the ratio k = N/n
  
  # Effective sample size
  n0 = (n * (k+1)) / (k*(1-rho^2)+1) 
  
  return(n0)
  
}

# Example: Effective sample size
n0_07 = n0(rho=0.7, n=1000, k=10)
n0_09 = n0(rho=0.9, n=1000, k=10)

# Effective sample size as a percentage of human sample size
round(100 * n0_07 / 1000)
round(100 * n0_09 / 1000)

# Percentage increase (n0-n)/n
round(100 * (n0_07 - 1000) / 1000, 2)
round(100 * (n0_09 - 1000) / 1000, 2)



# Define n=1 to let the effective sample size n0 be a multiple of the human sample size
n0_plotdata = expand.grid(
  n = 1,
  rho = c(0.1, 0.3, 0.5, 0.7, 0.9), 
  k = seq(0, 10, by=0.1)) %>% 
  mutate(n0 = n0(rho=rho, n=n, k=k))

# Plot ratio of sample sizes k against effective sample size
p_n0 = ggplot(n0_plotdata, aes(x = k, y = n0, color = factor(rho), linetype= factor(rho))) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(
    breaks = seq(0, 20, by=1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 100), 
    limits = c(NA,4),
  ) +
  scale_color_manual(
    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
    values = c("#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  ) +
  labs(
    x = "Number of silicon subjects for human subject N/n",
    y = bquote("Effective sample size"~n[0]~"as percentage of human sample size"~n),
  ) +
  guides(
    color = guide_legend(title = bquote("PPI correlation "~tilde(rho))), 
    linetype = guide_legend(title = bquote("PPI correlation "~tilde(rho)))
  ) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
) 

p_n0





#############################################################
# Plot theoretical ratio of PPI SE width against classical SE
#############################################################

# Define function that calculates the ratio of PPI SE to classical SE width
p_of_classic_se_ratio = function(rho, k) {
  # Define k as the ratio k = N/n
  # Then N/(N+n) = k/(1+k)
  
  # Ratio of PPI SE to classical SE  
  sqrt(1 - (k / (1+k)) * rho^2)
  
}

# Example: Ratio of PPI standard error to human sample standard error
se_07 = p_of_classic_se_ratio(rho=0.7, k=10)
se_09 = p_of_classic_se_ratio(rho=0.9, k=10)

# Effective sample size as a percentage of human sample size
round(100 * se_07, 2)
round(100 * se_09, 2)

# Percentage change
round(100 * (se_07 - 1) / 1, 2)
round(100 * (se_09 - 1) / 1, 2)


# Create dataset with example values for rho 
se_plotdata = expand.grid(
  rho = c(0.1, 0.3, 0.5, 0.7, 0.9), 
  k = seq(0, 10, by=0.1)) %>% 
  mutate(p_of_classic_se = p_of_classic_se_ratio(rho=rho, k=k))


# Plot ratio of sample sizes k against ratio of SEs
p_se = ggplot(se_plotdata, aes(x = k, y = p_of_classic_se, color = factor(rho), linetype= factor(rho))) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(
    breaks = seq(0, 10, by=1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = seq(0, 1, by=0.1)
  ) +
  scale_color_manual(
    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
    values = c("#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  ) +
  labs(
    x = "Number of silicon subjects for every human subject N/n",
    y = "PPI standard error as percentage of human subjects standard error     "
  ) +
  guides(
    color = guide_legend(title = bquote("PPI correlation "~tilde(rho))), 
    linetype = guide_legend(title = bquote("PPI correlation "~tilde(rho)))
  ) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
  ) 

p_se


########################################################################
# Create combined plot with effective sample size n0 and standard errors
########################################################################

# Create plot combining effective sample size and standard errors
p_legend = get_legend(p_se)

p_combined = ggpubr::ggarrange(
  p_n0, p_se, 
  nrow=1,
  labels = "auto", 
  vjust=1.8,
  common.legend = T, 
  legend = "bottom",
  legend.grob = p_legend
) 

p_combined

# Save plot 
ggsave(plot = p_combined, filename = "Figures/3_SEandN0.pdf", width=10, height=6)





##################
# Costs per sample
##################

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
ntok_out = 5    


# survey responses from 2097 users
nprompt = 22315 

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c_gpt35 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c_gpt4t = cost(nprompt=600000, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.01, price1k_out = 0.03)
writeLines(paste0("gpt-4-turbo: ",round(c_gpt4t,2),"$ for ",nprompt," API calls."))

# gpt-4o: 35.7$ for API 22315 calls
c_gpt4o = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.00250 , price1k_out = 0.00125)

writeLines(paste0("gpt-3.5-turbo-0125: ",round(c_gpt35,2),"$ for ",nprompt," API calls."))
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
                price1k_in = 0.00250 , price1k_out = 0.00125)

# Average costs across GPT models
avg_c1 = mean(c1_gpt35,c1_gpt4t,c1_gpt4o)

writeLines(paste0("gpt-4-turbo: ",c1_gpt4t,"$ for one API call."))
writeLines(paste0("gpt-3.5-turbo-0125: ",c1_gpt35,"$ for one API call."))
writeLines(paste0("gpt-4o: ",c1_gpt4o,"$ for one API call."))
writeLines(paste0("Average cost: ",avg_c1,"$ for one API call."))

# gpt-3.5-turbo-0125: 0.000204$ for one API call.
# gpt-4-turbo: 0.00408$ for one API call.
# gpt-4o: 0.00204$ for one API call.




###########################
# Cost saving requirements
###########################



# Function to calculate the cost of mixed subjects experiment as a percentage of human subjects experiment
pcost = function(.rho, .cf, .cY, .verbose=F){
  
  # cost of silicon sampling as a share of sampling human responses
  gamma =  .cf / .cY
  
  # Check is rho sufficiently large
  minimum_rho = (2*sqrt(gamma)) / (1+gamma)
  is_sufficient = .rho > minimum_rho
  
  # Print warning if not
  if(any(!is_sufficient)) print("Rho is not sufficiently large for some cases.")
  
  # Calculate the costs of a mixed subjects experiment as a percentage of a human subjects experiment
  pcost = 1 - .rho^2*(1-gamma) + 2*sqrt(gamma * .rho^2 * (1-.rho^2))
  
  # Percentage of cost of a human subjects experiment if silicon subjects incur no cost
  pcost_gamma0 = 1 - .rho^2*(1-0) + 2*sqrt(0 * .rho^2 * (1-.rho^2))
  
  if(.verbose){
    writeLines(paste0("Cost of silicon subject as percentage of the costs for a human subject: ", round(100 * gamma,2), "%"))
    writeLines(paste0("Silicon subjects responses affordable for human subject response: ", round(1/gamma)))
    writeLines(paste0("Minimum PPI correlation to save cost in a mixed subjects experiment: ", round(minimum_rho,3)))
    writeLines(paste0("Percentage of cost of a human subjects experiment: ", round(100*pcost,2),"%"))
    writeLines(paste0("Percentage of cost of a human subjects experiment if silicon subjects incur no cost: ", round(100*pcost_gamma0,2),"%"))
  }
  
  return(pcost)
}


# Cost of a LLM response 
c1_gpt4t = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.01, price1k_out = 0.03)

c1_gpt4t = 0.05

# Cost of a 5 minute survey response with Federal minimum wage $7.25/hour 
c1_human = 7.25/12
c1_human

# Cost of mixed subjects experiment as a percentage of human subjects experiment
psaving06 = pcost(.rho=0.6, .cf=c1_gpt4t, .cY=c1_human, .verbose = T)
round(100*psaving06,1)

psaving08 = pcost(.rho=0.8, .cf=c1_gpt4t, .cY=c1_human, .verbose = T)
round(100*psaving08,1)

# How cheap LLMs need to be for percent cost saving and rho
required_gamma = function(rho, psaving){
  
  c = psaving
  
  gamma = (rho^2 + c*rho^2 - rho^4 - 2*(c*rho^4 - c*rho^6)^0.5)/rho^4
  
  return(gamma)
}

# Calculate required gamma
gamma06 = required_gamma(0.6, psaving08)
c2_gpt4t = gamma06*c1_human

c2_gpt4t

psaving06_2 = pcost(.rho = 0.6, .cf=c2_gpt4t, .cY=c1_human, .verbose = T)

print(round(c1_gpt4t/c2_gpt4t, 2))

###############################################################################
# PPI experiments are cheaper for cheaper algorithms and higher PPI correlation
###############################################################################

title = bquote(paste("Cost of predicting a response as a\nshare of recruiting a human subject (", gamma, ")"))


# When gamma is not sufficiently large the mixed subjects experiment is 100% of the cost of a human subjects experiment
dd = expand.grid(
  rho = c(0.1, 0.3, 0.5, 0.7, 0.9),
  cf = 1/seq(1, 500, length.out = 200),
  cY = 1) %>% 
  mutate(
    pcost = pcost(.cf = cf, .cY = cY, .rho = rho), 
    gamma = cf / cY, 
    is_sufficient = ifelse(rho > (2*sqrt(gamma)) / (1+gamma), 1, 0), 
    pcost = ifelse(!is_sufficient, 1, pcost)
  )

p_cost = ggplot(dd, aes(1/gamma, pcost, color = factor(rho), linetype = factor(rho))) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = seq(0,1, by=0.1)
  ) +
  guides(
    color = guide_legend(title = bquote("PPI correlation "~tilde(rho))), 
    linetype = guide_legend(title = bquote("PPI correlation "~tilde(rho)))
  ) +
  scale_color_manual(
    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
    values = c("#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  ) +
  labs(
    x = bquote("Predictions affordable for every gold-standard observation"~1/gamma), 
    y = "Cost of mixed subjects as percentage of human subjects experiment   "
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
  ) 

p_cost

# Save plot
ggsave(plot = p_cost, filename = "Figures/3_PercentCostOfHumanSubjectsExperiment.pdf", width=7, height=5)


