source("Code/1_Functions.R")


#################################
# Calculate theoretical PPI width
#################################

p_of_classic_ci_ratio = function(rho, k) {
  # Define k as the ratio k = N/n
  # Then N/(N+n) = k/(1+k)
  
  # Ratio of PPI CI width to classical CI width 
  sqrt(1 - (k / (1+k)) * rho^2)
  
}
plotdata = expand.grid(rho = c(0.25, 0.5, 0.75), k = seq(0, 5, by=0.1)) %>% 
  mutate(p_of_classic_ci = p_of_classic_ci_ratio(rho=rho, k=k))

# Example 
example_ratio =  round(100 * p_of_classic_ci_ratio(rho=0.75, k=4), 1)
100 - example_ratio


ggplot(plotdata, aes(x = k, y = p_of_classic_ci, color = factor(rho))) +
  geom_line(linewidth=1) +
  labs(
    x = "Ratio of number of predictions to total sample size N/n",
    y = "Ratio of PPI CI width to classical CI width",
    color = bquote(tilde(rho))
  ) +
  scale_y_continuous(breaks = seq(0,1, by=0.05)) +
  scale_color_manual(breaks = c(0.25,0.5,0.75), values = c("#CC6677","#DDCC77","#4477AA")) +
  theme(legend.position = "bottom", text=element_text(size=12))
ggsave("Figures/7_WidthAsShareOfClassicCIWidth_ratio.pdf", width = 5, height=4.5)


# Define function to calculate proportion of classic CI width
p_of_classic_ci_nN = function(rho, n, N) {
  
  sqrt(1 - ((N / (N + n)) * rho^2))
  
}

# Set parameter values
rho_values = c(0.25, 0.5, 0.75)
n_values = c(50, 500, 1000)
N_values = seq(10, 2000, by=10)

# Create a data frame to hold all combinations of rho, n, and N
plotdata = expand.grid(rho = rho_values, n = n_values, N = N_values) %>%
  mutate(p_of_classic_ci = p_of_classic_ci_nN(rho, n, N))

ggplot(plotdata, aes(x = N, y = p_of_classic_ci, color = factor(rho))) +
  geom_line(linewidth=1) +
  facet_wrap(~n, labeller = as_labeller(function(l) paste0("n=",l))) +
  labs(
    x = "Number of predictions N",
    y = "Ratio of PPI CI width to classical CI width",
    color = bquote(tilde(rho))
  ) +
  scale_y_continuous(breaks = seq(0,1, by=0.05)) +
  scale_color_manual(breaks = c(0.25,0.5,0.75), values = c("#CC6677","#DDCC77","#4477AA")) +
  theme(legend.position = "bottom", text=element_text(size=12))
ggsave("Figures/7_WidthAsShareOfClassicCIWidth_absolute.pdf", width=6,height= 4.25)




##############################################
# Calculate AMCE for human and LLM predictions
##############################################

# Load data with responses from human subjects and predicted responses
mms = get_filepath("5_SurveySampleLLM.csv.gz") %>% 
  fread() %>% 
  PreprocessProfiles()


# Count NAs on dependent variables
dvs = c("Saved","gpt4turbo_wp_Saved","gpt4o_wp_Saved","gpt35turbo0125_wp_Saved")
summarise(mms,across(all_of(dvs), ~ sum(is.na(.))))


# Define function to calculate AMCE
calculate_amce = function(profiles, depvar){
  # Code adopted from https://osf.io/d7un2
  
  # Calculate AMCE and standard error like for Figure 2a in Awad et al. (2018)
  amce = profiles %>% 
    GetMainEffectSizesCustom(savedata=F, r=9, depvar=depvar) %>% 
    GetPlotData(isMainFig=T, r=9) %>% 
    mutate(conf.low  = amce - qnorm(0.975) * se, 
           conf.high = amce + qnorm(0.975) * se,
           dv = depvar) %>% 
    select(label, dv, amce, se, conf.low, conf.high)
  
  return(amce)
}


# AMCE for human subjects
main.Saved = calculate_amce(mms,"Saved")

# AMCE for GPT4 Turbo
main.gpt4turbo_wp_Saved = calculate_amce(mms,"gpt4turbo_wp_Saved")

# AMCE for GPT 4o
main.gpt4o_wp_Saved = calculate_amce(mms,"gpt4o_wp_Saved") 

# AMCE for GPT3.5 Turbo
main.gpt35turbo0125_wp_Saved = calculate_amce(mms,"gpt35turbo0125_wp_Saved") 
  
# AMCEs reported by Awad et al (2018) based responses from US and other countries 
main.Awad2018 = tribble(
  ~amce, ~label,
  0.061, "Intervention",
  0.097, "Barrier",
  0.353, "CrossingSignal",
  0.119, "Gender",
  0.160, "Fitness",
  0.345, "Social Status",
  0.497, "Age",
  0.651, "Utilitarian",
  0.585, "Species") %>% 
  mutate(dv="Awad2018")

# Create labels for plot
cols = tribble(
  ~dv,                       ~Color,    ~Label, 
  "gpt4turbo_wp_Saved",      "#223c56", "GPT-4 Turbo ",
  "gpt4o_wp_Saved",          "#4477AA", "GPT-4o ", 
  "gpt35turbo0125_wp_Saved", "#9fbcd9", "GPT-3.5 Turbo ",
  "Saved",                   "#DDCC77", " \nStratified Moral\nMachine Sample\n ",
  "Awad2018",                "#CC6677", " \nAwad et al.\n(2018)\n "
  ) %>%  
mutate(dv = factor(dv,ordered = T))


# Create dataframe storing all AMCEs
amces = main.Saved %>% 
  bind_rows(main.Awad2018) %>% 
  bind_rows(main.gpt4turbo_wp_Saved) %>% 
  bind_rows(main.gpt4o_wp_Saved) %>% 
  bind_rows(main.gpt35turbo0125_wp_Saved) %>% 
  mutate(dv = factor(dv,levels=cols$dv))


# Create bar plot with AMCEs and 95% CIs
ggplot(amces, aes(x=amce, y=label, xmin=conf.low, xmax=conf.high, fill=dv)) + 
  geom_col(position=position_dodge(width=0.8)) +
  geom_errorbar(position=position_dodge(width=0.8),width=.2,color="darkgrey") +
  scale_fill_manual(breaks=cols$dv, values=cols$Color, labels=cols$Label) + 
  labs(fill = "Dataset",x="AMCE with 95% confidence intervals", y="Attribute of Scenario") +
  theme(axis.text.y = element_text(size = 8.7))

ggsave(filename="Figures/7_AMCEs.pdf",width=7,height=5)


################################
# Compare AMCE estimate from PPI
################################

# Load PPI estimates
dfsim = read_csv("Data/6_ResultsPPI.csv.gz") %>% 
  left_join(main.Saved, by=c("x"="label")) %>% 
  mutate(param_in_ppi_ci    = ifelse(amce >= conf_low_ppi & amce <= conf_high_ppi, 1, 0),
         param_in_pooled_ci = ifelse(amce >= conf_low_pooled & amce <= conf_high_pooled, 1, 0),
         width_ppi_ci = conf_high_ppi - conf_low_ppi,
         width_pooled_ci = conf_high_pooled - conf_low_pooled, 
         width_ols_ci = conf_high_ols - conf_low_ols)

# NA values
summarise(dfsim, across(everything(), ~ sum(is.na(.))))

# average over repetitions
dfsim_w = dfsim %>% 
  group_by(x,y,n,N) %>% 
  summarize(coverage_ppi = 100 * (sum(param_in_ppi_ci == 1) / sum(param_in_ppi_ci %in% 0:1)),
            coverage_pooled = 100 * (sum(param_in_pooled_ci == 1) / sum(param_in_pooled_ci %in% 0:1)),
            mean_width_ppi_ci = mean(width_ppi_ci,na.rm=T), 
            mean_width_pooled_ci = mean(width_pooled_ci,na.rm=T), 
            mean_width_ols_ci = mean(width_pooled_ci,na.rm=T), 
            mean_width_ols_ci = mean(width_ols_ci, na.rm=T)) %>% 
  mutate(ratio_ols_ppi_ci =  mean_width_ppi_ci / mean_width_ols_ci, 
         ratio_N_n = N/n)

plot_results = function(.predictors, .ns, .Nmax, .model){
  
  dd = dfsim_w %>% 
    filter(x %in% .predictors, n %in% .ns, y == .model, N <= .Nmax, 
           x != "Social Status")
  
  brewer_palette = "Set2"
  
  # plot confidence interval width for increasing N
  p1 = dd %>% 
    pivot_longer(cols = ratio_ols_ppi_ci) %>% 
    ggplot(aes(N, value, color = x)) + 
    geom_line() +
    facet_grid(~ n,labeller=labeller(n=function(lab) paste0("n=",lab))) +
    labs(linetype="Language Model", y = "Ratio of PPI CI width to classical CI width",
         color="Predictor", x="N", x = "N\n(Number of LLM predictions)") +
    scale_color_brewer(palette = brewer_palette) 
    
  
  
  # plot percentage of time CIs cover best parameter estimate for increasing N
  p2 = dd %>% 
    pivot_longer(cols = c(coverage_ppi,coverage_pooled),values_to="coverage",names_to="method") %>% 
    mutate(method = method %>% 
             str_extract("ppi|pooled") %>% 
             factor(levels = c("pooled","ppi"), labels = c("Pooled","PPI"))) %>% 
    ggplot(aes(N, coverage, color=x,linetype=method)) + 
    geom_line() +
    scale_y_continuous(limits = c(0,100)) + 
    facet_grid(~ n,labeller=labeller(n=function(lab) paste0("n=",lab))) +
    labs(color="Method",y = "Coverage in %", linetype="Method                ") +
    scale_color_brewer(palette = brewer_palette) +
    guides(color = "none")
  
  
  p = ggpubr::ggarrange(p1,p2,nrow=2,common.legend = F,legend = "right") 
  print(p)
  
  ggsave(paste0("Figures/7_SimulationResults_", .model,".pdf"), plot=p, width=7,height=6)
}

models = unique(dfsim_w$y)
Xs = unique(dfsim_w$x)
for (i in seq_along(models)){
  plot_results(.predictors = Xs, 
               .ns = c(50,500), 
               .Nmax = 2000,
               .model = models[i])
}


  

#########################
# Ratio 
#########################



plot_results_ratio = function(.predictors, .n, .Nmax, .model){
  
  colors <- tribble(
    ~Variable,       ~Code,
    "Age",           "#1170AAFF",
    "Barrier",       "#FC7D0BFF",
    "CrossingSignal","#C8D0D9FF",
    "Fitness",       "#57606CFF",
    "Gender",        "#5FA2CEFF",
    "Intervention",  "#C85200FF",
    "Species",       "#FFBC79FF",
    "Utilitarian",   "#A3CCE9FF",
    "Social Status", "#7B848FFF"
  )
  
  dd = dfsim_w %>% 
    filter(x %in% .predictors, n ==.n, y == .model, N <= .Nmax, 
           x != "Social Status")
  
  brewer_palette = "Set2"
  
  # plot confidence interval width for increasing N
  p1 = dd %>% 
    pivot_longer(cols = ratio_ols_ppi_ci) %>% 
    ggplot(aes(ratio_N_n, value, color = x)) + 
    geom_line() +
    labs(linetype="Language Model", y = "Ratio of PPI CI width to classical CI width",
         color="Predictor", x = "Ratio of number of predictions to total sample size N/n") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code)
  
  
  # plot percentage of time CIs cover best parameter estimate for increasing N
  p2 = dd %>% 
    pivot_longer(cols = c(coverage_ppi,coverage_pooled),values_to="coverage",names_to="method") %>% 
    mutate(method = method %>% 
             str_extract("ppi|pooled") %>% 
             factor(levels = c("pooled","ppi"), labels = c("Pooled","PPI"))) %>% 
    ggplot(aes(ratio_N_n, coverage, color=x,linetype=method)) + 
    geom_line() +
    scale_y_continuous(limits = c(0,100)) + 
    labs(x = "Ratio of number of predictions to total sample size N/n",
         color="Method",y = "Coverage in %", linetype="Method                ") +
    guides(color = "none") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code)
  
  
  p = ggpubr::ggarrange(p1,p2,nrow=2,legend = "right") 
  print(p)
  ggsave(paste0("Figures/7_SimulationResults_", .model,"_Nn_n",.n,".pdf"), plot=p, width=7,height=6)
}

models = unique(dfsim_w$y)
Xs = unique(dfsim_w$x)
ns = c(50,500)
for (n in seq_along(ns)) {
  for (i in seq_along(models)){
    plot_results_ratio(.predictors = Xs, 
                       .n = ns[n], 
                       .Nmax = ns[n] * 5,
                       .model = models[i])
  }
}

