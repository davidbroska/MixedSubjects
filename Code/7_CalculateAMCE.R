source("Code/1_Functions.R")


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
  0.097, "Relation to AV",
  0.353, "Law",
  0.119, "Gender",
  0.160, "Fitness",
  0.345, "Social Status",
  0.497, "Age",
  0.651, "No. Characters",
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
         width_pooled_ci = conf_high_pooled - conf_low_pooled)

# NA values
summarise(dfsim, across(everything(), ~ sum(is.na(.))))

# average over repetitions
dfsim_w = dfsim %>% 
  group_by(x,y,n,N) %>% 
  summarize(coverage_ppi = 100 * (sum(param_in_ppi_ci == 1) / sum(param_in_ppi_ci %in% 0:1)),
            coverage_pooled = 100 * (sum(param_in_pooled_ci == 1) / sum(param_in_pooled_ci %in% 0:1)),
            mean_width_ppi_ci = mean(width_ppi_ci,na.rm=T), 
            mean_width_pooled_ci = mean(width_pooled_ci,na.rm=T))

# plot confidence interval width for increasing N
p1 = dfsim_w %>% 
  pivot_longer(cols = c(mean_width_ppi_ci,mean_width_pooled_ci),values_to="width",names_to="method") %>% 
  mutate(method = str_extract(method,"ppi|pooled")) %>% 
  ggplot(aes(N,width,color=method,fill=y,linetype=y)) + 
  geom_line() +
  facet_grid(~ n,labeller=labeller(n=function(lab) paste0("n=",lab))) +
  labs(color="Method", linetype="Language Model", 
       color="Method", y="95% CI width", x="N")

# plot percentage of time CIs cover best parameter estimate for increasing N
p2 = dfsim_w %>% 
  pivot_longer(cols = c(coverage_ppi,coverage_pooled),values_to="coverage",names_to="method") %>% 
  mutate(method = str_extract(method,"ppi|pooled")) %>% 
  ggplot(aes(N,coverage,color=method,fill=y,linetype=y)) + 
  geom_line() +
  scale_y_continuous(limits = c(0,100)) + 
  facet_grid(~ n,labeller=labeller(n=function(lab) paste0("n=",lab))) +
  labs(color="Method", linetype="Language Model", 
       y="Coverage in %", x="N\n(Number of LLM predictions)")

ggpubr::ggarrange(p1,p2,nrow=2,common.legend = T,legend = "right")
ggsave("Figures/7_SimulationResults.pdf",width=7,height=6)




