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
c_gpt4t = cost(nprompt=5000, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c_gpt4o = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.00250 , price1k_out = 0.00125)

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
                price1k_in = 0.00250 , price1k_out = 0.00125)

avg_c1 = mean(c1_gpt35,c1_gpt4t,c1_gpt4o)

writeLines(paste0("gpt-3.5-turbo-0125: ",c1_gpt35,"$ for one API call."))
writeLines(paste0("gpt-4-turbo: ",c1_gpt4t,"$ for one API call."))
writeLines(paste0("gpt-4o: ",c1_gpt4o,"$ for one API call."))
writeLines(paste0("Average cost: ",avg_c1,"$ for one API call."))

# gpt-3.5-turbo-0125: 0.000204$ for one API call.
# gpt-4-turbo: 0.00408$ for one API call.
# gpt-4o: 0.00204$ for one API call.




###########################
# Cost saving requirements
###########################

# Cost of a 12 minute survey response with California minimum wage $16.00/hour 
c1_human = 12 * 16/60                # $3.20    
c1_llm   = round(13*c1_gpt35,3)     # $0.002652 to $0.003

# Function to calculate the percent saved when complementing human with silicon subjects
pcost = function(.rho, .cf, .cY, .verbose=F){
  
  # cost of silicon sampling as a share of sampling human responses
  gamma =  .cf / .cY
  
  
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
psaving = pcost(.rho=0.35, .cf=c1_llm, .cY=c1_human, .verbose = T)
psaving

# 165.63$ saved in a n=500 study
500*c1_human * psaving



##################################################################################
# Plot effective sample size against N for different values of the PPI correlation
##################################################################################

# Define function that calculates the effective sample size
n0 = function(rho, k) {
  # Define k as the ratio k = N/n and n=1
  n=1
  
  # Effective sample size
  n0 = (n * (k+1)) / (k*(1-rho^2)+1) 
  
  # Percent increase over human subjects sample size n is equal to n0-1
  #p_increase = (n0 - n) / n
  
  return(n0)
  
}

# Create dataset with example values for rho 
n0_plotdata = expand.grid(
  rho = c(0.1, 0.3, 0.5, 0.7, 0.9), 
  k = seq(0, 10, by=0.1)
) %>% 
  mutate(n0 = n0(rho=rho, k=k))

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
    x = "Number of predictions for every gold-standard observation N/n",
    y = bquote("Effective sample size"~n[0]~"as percentage of human sample size"~n),
  ) +
  guides(
    color = guide_legend(title = bquote("PPI correlation "~tilde(rho))), 
    linetype = guide_legend(title = bquote("PPI correlation "~tilde(rho)))
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
) 
p_n0
ggsave(plot = p_n0, filename = "Figures/3_EffectiveSampleSize.pdf", width=7, height=5)




#############################################################
# Plot theoretical ratio of PPI CI width against classical CI
#############################################################

# Define function that calculates the ratio of PPI CI to classical CI width
p_of_classic_ci_ratio = function(rho, k) {
  # Define k as the ratio k = N/n
  # Then N/(N+n) = k/(1+k)
  
  # Ratio of PPI CI width to classical CI width 
  sqrt(1 - (k / (1+k)) * rho^2)
  
}

# Create dataset with example values for rho 
ci_plotdata = expand.grid(
  rho = c(0.1, 0.3, 0.5, 0.7, 0.9), 
  k = seq(0, 10, by=0.1)
) %>% 
  mutate(p_of_classic_ci = p_of_classic_ci_ratio(rho=rho, k=k))

# Example from article
example_ratio =  round(100 * p_of_classic_ci_ratio(rho=0.75, k=4), 1)
100 - example_ratio

# Plot ratio of sample sizes k against ratio of CI widths
p_ci = ggplot(ci_plotdata, aes(x = k, y = p_of_classic_ci, color = factor(rho), linetype= factor(rho))) +
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
    x = "Number of predictions for every gold-standard observation N/n",
    y = "PPI SE as percentage of classical SE"
  ) +
  guides(
    color = guide_legend(title = bquote("PPI correlation "~tilde(rho))), 
    linetype = guide_legend(title = bquote("PPI correlation "~tilde(rho)))
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
  ) 
p_ci
ggsave(plot = p_ci, filename = "Figures/3_SeAsPercentageOfShareOfClassicSe.pdf", width=7, height=5)





###############################################################################
# PPI experiments are cheaper for cheaper algorithms and higher PPI correlation
###############################################################################

title = bquote(paste("Cost of predicting a response as a\nshare of recruiting a human subject (", gamma, ")"))

dd = expand.grid(rho = c(0.1, 0.3, 0.5, 0.7, 0.9),
                 cf = 1/seq(1, 500, length.out = 200),
                 cY = 1) %>% 
  mutate(pcost = pcost(.cf = cf, .cY = cY, .rho = rho), 
         gamma = cf / cY, 
         gamma_formatted = paste0(100*gamma, "%"),
         is_sufficient = ifelse(rho > (2*sqrt(gamma)) / (1+gamma), 1, 0), 
         pcost = ifelse(!is_sufficient, 0, pcost)
  ) %>% 
  filter(!is.na(pcost))

ggplot(dd, aes(1/gamma, 1-pcost, color = factor(rho), linetype = factor(rho))) +
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
    y = "Cost of PPI experiment as % of classical experiment"
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(4,"lines"), 
    legend.key.height = unit(2, "lines")
  ) 
ggsave(filename = "Figures/3_PercentCostOfHumanSubjectsExperiment.pdf", width=7, height=5)


