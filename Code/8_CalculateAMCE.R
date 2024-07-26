# Load custom functions
source("Code/1_Functions.R")


colors = tribble(
    ~Variable,       ~Code,        ~Label,
    "Species",       "#FFBC79FF",  "Sparing humans\nvs animals",
    "Utilitarian",   "#A3CCE9FF",  "Sparing more characters\nvs fewer",
    "Age",           "#1170AAFF",  "Sparing the young\nvs old",
    "Gender",        "#5FA2CEFF",  "Sparing women\nvs men",
    "CrossingSignal","#FC7D0BFF",  "Sparing the lawful\nvs unlawful",
    "Fitness",       "#57606CFF",  "Sparing the fit\nvs the large",
    "Barrier",       "#C8D0D9FF",  "Sparing pedestrians\nvs passengers",
    "Intervention",  "#C85200FF",  "Preference for inaction\nvs intervention",
    "Social Status", "#7B848FFF",  "Sparing high status\nvs low status"
  )


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
  mutate(
    dv = factor(dv,levels=cols$dv), 
    label = factor(label,levels=colors$Variable[nrow(colors):1], labels=colors$Label[nrow(colors):1])
  )


# Create bar plot with AMCEs and 95% CIs
ggplot(amces, aes(x=amce, y=label, xmin=conf.low, xmax=conf.high, fill=dv)) + 
  geom_col(position=position_dodge(width=0.8)) +
  geom_errorbar(position=position_dodge(width=0.8), width=.2, color="darkgrey") +
  scale_fill_manual(breaks=cols$dv, values=cols$Color, labels=cols$Label) + 
  labs(fill = "Dataset",x="AMCE with 95% confidence intervals", y="Attribute of Scenario") +
  theme(axis.text.y = element_text(size = 8.7))

ggsave(filename="Figures/8_AMCEs.pdf",width=7,height=5)




###############################################################
# Compare AMCE estimate from PPI with WLS in Moral Machine Data
###############################################################

# Load association between predicted and observed responses
rhos = read_csv("Data/7_rho.csv", col_select = c("x","y","rho"))

# Load PPI estimates
dfsim = read_csv("Data/7_ResultsPPI.csv.gz") %>% 
  left_join(main.Saved, by=c("x"="label")) %>% 
  mutate(param_in_ppi_ci    = ifelse(amce >= conf_low_ppi & amce <= conf_high_ppi, 1, 0),
         param_in_pooled_ci = ifelse(amce >= conf_low_pooled & amce <= conf_high_pooled, 1, 0),
         ppi_over_amce_ci = (conf.low <= conf_high_ppi) * (conf.high >= conf_low_ppi),
         pooled_over_amce_ci = (conf.low <= conf_high_pooled) * (conf.high >= conf_low_pooled),
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
            overlap_ppi = 100 * (sum(ppi_over_amce_ci==1) / sum(ppi_over_amce_ci %in% 0:1)),
            overlap_pooled = 100 * (sum(pooled_over_amce_ci==1) / sum(pooled_over_amce_ci %in% 0:1)),
            mean_width_ppi_ci = mean(width_ppi_ci,na.rm=T), 
            mean_width_pooled_ci = mean(width_pooled_ci,na.rm=T), 
            mean_width_ols_ci = mean(width_pooled_ci,na.rm=T), 
            mean_width_ols_ci = mean(width_ols_ci, na.rm=T)) %>% 
  mutate(ratio_ols_ppi_ci =  mean_width_ppi_ci / mean_width_ols_ci, 
         ratio_N_n = N/n)



plot_results_ratio = function(.predictors, .n, .Nmax, .model, .rhos){

  
  dd = dfsim_w %>% 
    filter(x %in% .predictors, n ==.n, y == .model, N <= .Nmax, 
           x != "Social Status")
  
  brewer_palette = "Set2"
  
  format_digit = function(.number){
    .number %>% 
      round(2) %>% 
      as.character() %>% 
      sub("^0\\.", ".", .) %>% 
      ifelse(nchar(.) == 2, paste0(.,"  "), .)
  }
  
  r = dd %>% 
    group_by(x,y) %>% 
    slice(which.max(ratio_N_n)) %>% 
    left_join(.rhos, by = c("x","y")) %>% 
    mutate(rho = format_digit(rho))
  
  asize = 3
  xnudge = 5.23
  
  add_labs = function(.plot, .var, .ynudge=0){
    
    lab = deparse(bquote(" "*tilde(rho)==.(filter(r,x==.var)$rho)))
    
    y = r %>% 
      filter(x==.var) %>% 
      pull(ratio_ols_ppi_ci)
    
    annotated = .plot + 
      annotate("text",label=lab,y=y+.ynudge, size=asize, 
               parse=T, x=xnudge,family = "serif") 
      
    return(annotated)
  }
  
  # plot confidence interval width for increasing N
  p1 = dd %>% 
    pivot_longer(cols = ratio_ols_ppi_ci, values_to = "ratio_ols_ppi_ci") %>% 
    ggplot(aes(ratio_N_n, ratio_ols_ppi_ci, color = x)) + 
    geom_line() +
    labs(linetype="Language Model", y = "PPI CI width as % of classical CI",
         color="Independent variable", x = "Number of predictions for every gold-standard observation N/n") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code, labels = colors$Label) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 100), 
      breaks = seq(0,1,0.01)
    ) + 
    theme(
      legend.key.height = unit(0.75, "cm"), 
      panel.grid.major = element_line(size = 0.2),  
      panel.grid.minor = element_line(size = 0.1)) 
  
  p1 = p1 %>% 
    add_labs("Age",.ynudge = -0.001) %>% 
    add_labs("Barrier",.ynudge = -0.0005) %>% 
    add_labs("CrossingSignal",.ynudge = -0.0015) %>% 
    add_labs("Fitness") %>% 
    add_labs("Gender", .ynudge = 0.001) %>% 
    add_labs("Intervention") %>% 
    add_labs("Species") %>% 
    add_labs("Utilitarian",.ynudge = 0.0005) 
  
  # plot percentage of time CIs cover best parameter estimate for increasing N
  p2 = dd %>% 
    #pivot_longer(cols = c(coverage_ppi,coverage_pooled),values_to="coverage",names_to="method") %>% 
    pivot_longer(cols = c(overlap_ppi,overlap_pooled),values_to="coverage",names_to="method") %>% 
    mutate(method = method %>% 
             str_extract("ppi|pooled")) %>% 
    ggplot(aes(ratio_N_n, coverage, color=x,linetype=method)) + 
    geom_line() +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1), 
      limits = c(0,100)
    ) + 
    labs(x = "Number of predictions for every gold-standard observation N/n", color="Method", 
         y="% of CIs that cover parameter", linetype="Method                            ") +
    guides(color = "none") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code) +
    scale_linetype_manual(
      breaks=c("ppi","pooled"), 
      labels=c("\nPrediction-powered\ninference (PPI)\n","\nRegression on\npooled sample\n"), 
      values=c("dashed","solid")) +
    theme(panel.grid.major = element_line(size = 0.2), panel.grid.minor = element_line(size = 0.1)) +
    annotate("text",label= " ", parse=T, x=xnudge, y=filter(r,x=="Age")$ratio_ols_ppi_ci,size=asize)
  
  
  p = ggpubr::ggarrange(p1,p2,nrow=2,legend = "right", labels = "auto", vjust=1) 
  print(p)
  ggsave(filename = paste0("Figures/8_SimulationResults_", .model,"_Nn_n",.n,".pdf"), 
         plot=p, width=7, height=6)
}





models = unique(dfsim_w$y)
models = "gpt4turbo_wp_Saved"
Xs = unique(dfsim_w$x)
ns = c(100,500)
for (n in seq_along(ns)) {
  for (i in seq_along(models)){
    plot_results_ratio(.predictors = Xs, 
                       .n = ns[n], 
                       .Nmax = ns[n] * 5,
                       .model = models[i], 
                       .rhos = rhos)
  }
}
 
