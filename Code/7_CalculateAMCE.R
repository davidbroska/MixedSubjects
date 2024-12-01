# Load custom functions
source("Code/RFunctions.R")


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



gpt4t = get_filepath("4_gpt4turbo_wp_20241118.csv.gz") %>% 
  fread() %>% 
  PreprocessProfiles()




# Count NAs on dependent variables
summarise(gpt4t,across(all_of(c("Saved","gpt4turbo_wp_Saved")), ~ sum(is.na(.))))


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
main.Saved = calculate_amce(gpt4t,"Saved")

# Output referenced in Python script
mutate(main.Saved, across(where(is.numeric), ~ round(.,3))) %>% kable()

# Save csv
write_csv(main.Saved, paste0(get_filepath("Data"),"/7_AmceParamsSimulationR.csv.tar"))


# AMCE for GPT4 Turbo
main.gpt4turbo_wp_Saved = calculate_amce(gpt4t,"gpt4turbo_wp_Saved")

  
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
  "Awad2018",                "#DDCC77", " \nAwad et al.\n(2018)\n ",
  "gpt4turbo_wp_Saved",      "#4477AA", " \nSilicon Subjects\nU.S. Sample\n",
  "Saved",                   "#CC6677", " \nHuman Subjects\nU.S. Sample\n ",
) %>%  
  mutate(dv = factor(dv,ordered = T))


# Create dataframe storing all AMCEs
amces = main.Saved %>% 
  bind_rows(main.Awad2018) %>% 
  bind_rows(main.gpt4turbo_wp_Saved) %>% 
  mutate(
    dv = factor(dv,levels=cols$dv), 
    label = factor(label,levels=colors$Variable[nrow(colors):1], labels=colors$Label[nrow(colors):1])
  )


# Create bar plot with AMCEs and 95% CIs
ggplot(amces, aes(x=amce, y=label, xmin=conf.low, xmax=conf.high, fill=dv)) + 
  geom_col(position=position_dodge(width=0.8)) +
  geom_errorbar(
    position=position_dodge(width=0.8),
    width=.2, 
    color="darkgrey"
  ) +
  scale_fill_manual(
    breaks=cols$dv,
    values=cols$Color, 
    labels=cols$Label
  ) + 
  labs(
    fill = "Dataset",
    x="AMCE with 95% confidence intervals", 
    y="Attribute of Scenario"
  ) +
  guides(
    fill = guide_legend(reverse = T)
  ) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text = element_text(size = 9))

ggsave(filename="Figures/8_AMCEs.pdf",width=7,height=5)




#####################################################################
# Simulation to compare AMCEs from PPI with WLS in Moral Machine Data
#####################################################################

# Load association between predicted and observed responses
rhos = read_csv("Data/7_rho.csv", col_select = c("x","y","ppi_corr"))

# Load PPI estimates
dfsim = read_csv("Data/7_ResultsPPI.csv.gz") %>% 
  mutate(
    ratio_ppi_hum_se = se_ppi / se_hum,
    ratio_ppi_hum_se_theory = sqrt(1-ppi_corr^2 * N/(n+N)), 
    ratio_sil_hum_se = se_sil / se_hum,
    n0_ppi = n*(1/ratio_ppi_hum_se)^2,
    n0_theory = n*(n+N)/(n+N*(1-ppi_corr^2))
)

# furhter summary of increase in precision
dfsim %>% 
  filter(N == max(dfsim$N)) %>% 
  select(x,  ppi_corr, n0_ppi) %>% 
  arrange(desc(ppi_corr))

dfsim %>% 
  filter(N == max(dfsim$N)) %>% 
  ggplot(aes(beta_hum, beta_sil)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

dfsim %>% 
  filter(N == max(dfsim$N)) %>% 
  ggplot(aes(beta_hum, beta_ppi)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))


# Check for NA values
summarise(dfsim, across(everything(), ~ sum(is.na(.))))





plot_results_ratio = function(.predictors, .n, .Nmax, .model, .rhos){

  
  dd = dfsim %>% 
    filter(x %in% .predictors, n ==.n, y == .model, N <= .Nmax, 
           x != "Social Status", 
           y == "gpt4turbo_wp_Saved_1")
  
  # bias
  d1 = dd %>% 
    pivot_longer(cols = c(bias_ppi,bias_sil), values_to="bias",names_to="method") %>% 
    mutate(method = method %>% str_extract("ppi|sil|hum")) 
  
  # precision
  d2 = dd %>% 
    pivot_longer(cols = c(ratio_sil_hum_se,ratio_ppi_hum_se), values_to="ratio",names_to="method") %>% 
    mutate(method = method %>% str_extract("ppi|sil|hum")) 
  
  # coverage
  d3 = dd %>% 
    pivot_longer(cols = c(coverage_ppi,coverage_sil),values_to="coverage",names_to="method") %>% 
    mutate(method = method %>% str_extract("ppi|sil|hum")) 
  
  brewer_palette = "Set2"
  
  format_digit = function(.number){
    .number %>% 
      round(2) %>% 
      as.character() %>% 
      sub("^0\\.", ".", .) %>% 
      ifelse(nchar(.) == 2, paste0(.,"  "), .)
  }
  
  r = d2 %>% 
    group_by(x,y) %>% 
    slice(which.max(N)) %>% 
    #left_join(.rhos, by = c("x","y")) %>% 
    mutate(ppi_corr = format_digit(ppi_corr))
  
  asize = 3.65
  xnudge = .Nmax/.n + .383
  
  add_labs = function(.plot, .var, .ynudge=0){
    
    lab = deparse(bquote(" "*tilde(rho)==.(filter(r,x==.var)$ppi_corr)))
    
    y = r %>% 
      filter(x==.var) %>% 
      pull(ratio)
    
    annotated = .plot + 
      annotate("text",label=lab,y=y+.ynudge, size=asize, 
               parse=T, x=xnudge,family = "serif") 
      
    return(annotated)
  }
  
  # bias plot
  p1 = d1 %>% 
    ggplot(aes(N, bias, color=x, linetype=method)) + 
    geom_line() +
    labs(linetype="Language Model", y = "PPI CI width as % of classical CI",
         color="Independent variable", x = "Number of predictions for every gold-standard observation N/n") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code, labels = colors$Label) +
    # scale_x_continuous(
    #   breaks = seq(0, .Nmax/.n, 1), 
    #   limits = c(0,5.5)
    # ) +
    theme(
      legend.key.height = unit(0.75, "cm"), 
      panel.grid.major = element_line(linewidth = 0.2),  
      panel.grid.minor = element_line(linewidth = 0.1)
    ) 
  p1

  # precision plot
  p2 = d2 %>% 
    ggplot(aes(N, ratio, color = x, linetype=method)) + 
    geom_line() +
    labs(linetype="Language Model", y = "PPI CI width as % of classical CI",
         color="Independent variable", x = "Number of predictions for every gold-standard observation N/n") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code, labels = colors$Label) +
    guides(color = "none") +
    # scale_x_continuous(
    #   breaks = seq(0, .Nmax/.n, 1), 
    #   limits = c(0,5.5)
    # ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 100),
      breaks = seq(0,1,0.1), 
      limits = c(0,1)
    ) +
    theme(
      legend.key.height = unit(0.75, "cm"), 
      panel.grid.major = element_line(linewidth = 0.2),  
      panel.grid.minor = element_line(linewidth = 0.1)
    ) 
  
  p2 = p2 %>% 
    add_labs("Species") %>% 
    add_labs("Utilitarian",.ynudge = 0.0005) %>%  
    add_labs("Age",.ynudge = -0.0005) %>% 
    add_labs("Gender", .ynudge = 0.00085) %>% 
    add_labs("CrossingSignal", .ynudge = -0.0012) %>% 
    add_labs("Fitness", .ynudge = 0.001) %>% 
    add_labs("Barrier",.ynudge = -0.0006) %>% 
    add_labs("Intervention")  
  
  print(p2)
  

  # coverage plot
  p3 = d3 %>% 
    ggplot(aes(N, 100*coverage, color=x, linetype=method)) + 
    geom_line() +
    # scale_x_continuous(
    #   breaks = seq(0, .Nmax/.n, 1), 
    #   limits = c(0,5.5)
    # ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1), 
      breaks = c(0,20,40,60,80,100),
      limits = c(0,100)
    ) + 
    labs(x = "Number of predictions for every gold-standard observation N/n", color="Method", 
         y="% of CIs that cover parameter", linetype="Method                            ") +
    guides(color = "none", linetype="none") +
    scale_color_manual(breaks = colors$Variable, values = colors$Code) +
    scale_linetype_manual(
      breaks=c("ppi","sil","hum"), 
      labels=c("\nMixed Subjects with PPI\n","\nSilicon Sampling\n","\nHuman Subjects\n"), 
      values=c("dashed","solid","dotted")) +
    theme(
      panel.grid.major = element_line(linewidth = 0.2), 
      panel.grid.minor = element_line(linewidth = 0.1),
      legend.key.width = unit(1.25, "cm")  
    ) #+ 
    #annotate("text",label= " ", parse=T, x=xnudge, y=filter(r,x=="Age")$ratio_hum_ppi_se,size=asize)
  
  p3
  
  p = ggpubr::ggarrange(p1,p2,p3,nrow=3,legend = "right", labels = "auto", vjust=1) 
  print(p)
  ggsave(filename = paste0("Figures/8_SimulationResults_", .model,"_Nn_n",.n,".pdf"), 
         plot=p, width=7, height=6)
}





models = unique(dfsim$y)
models = "gpt4turbo_wp_Saved_1"
Xs = unique(dfsim$x)
ns = c(500)

i = n = 1
.predictors = Xs
.n = ns[n]
.Nmax = ns[n] * 5
.model = models[i] 
.rhos = rhos

for (n in seq_along(ns)) {
  for (i in seq_along(models)){
    plot_results_ratio(.predictors = Xs, 
                       .n = ns[n], 
                       .Nmax = ns[n] * 5,
                       .model = models[i], 
                       .rhos = rhos)
  }
}


###############################################################
# Analysis of prediction error 
###############################################################


scipen(99999)
scale2 = function(.var){
  # scale by 2 standard deviations 
  # then the regression coefficient regression represents the change in the DV by 2 SDs
  (.var - mean(.var,na.rm=T)) / (2*sd(.var,na.rm=T))
}

sd(df$Review_political)
# Load data
df = read_csv("Data/5_SurveySampleLLM.csv.gz") %>% 
  filter(!is.na(gpt4turbo_wp_Saved_1)) %>% 
  mutate(error = abs(gpt4turbo_wp_Saved_1-Saved),
         UserID = factor(UserID), 
         Review_educationBracket = Review_educationBracket %>% factor() %>% relevel(ref = "Some college"), 
         Review_ContinuousIncome2Sd = scale2(Review_ContinuousIncome),
         Review_age2Sd = scale2(Review_age),
         Review_religious2Sd = scale2(Review_religious),
         Review_political2Sd = scale2(Review_political),
         ExtremeReligion = abs(Review_religious - mean(Review_religious)), 
         ExtremePolitical = abs(Review_political2Sd - mean(Review_political2Sd))
) 

range(df$error)

df$error
sum(is.na(df$error))

m = lm(error ~ Review_age+Review_education+Review_income+Review_political+Review_religious +
         Intervention, df)
summary(m)

library(fixest)
m = feols(
  error ~ Review_age2Sd + Review_educationBracket + Review_ContinuousIncome2Sd + Review_political2Sd + Review_religious2Sd + 
    Review_religious2Sd^2 + 
    Intervention + Barrier + CrossingSignal + PedPed + ScenarioType + 
    NumberOfCharacters + Man + Woman + Pregnant + Stroller + OldMan + OldWoman + Boy + Girl + Homeless + LargeWoman + LargeMan + 
    Criminal + MaleExecutive + FemaleExecutive + FemaleAthlete + MaleAthlete + FemaleDoctor + MaleDoctor + Dog + 
    DescriptionShown + LeftHand + ScenarioOrder + Template 
    , cluster = "ResponseID",
  data = df
)
summary(m)


m = feols(
  error ~ Review_age2Sd + Review_educationBracket + Review_ContinuousIncome2Sd + Review_political2Sd + Review_religious2Sd + 
    Review_religious2Sd^2 + 
    Intervention + Barrier + CrossingSignal + PedPed + ScenarioType + 
    Intervention*Review_ContinuousIncome2Sd + Barrier*Review_ContinuousIncome2Sd + CrossingSignal*Review_ContinuousIncome2Sd + PedPed*Review_ContinuousIncome2Sd + ScenarioType*Review_ContinuousIncome2Sd + 
    Intervention*Review_educationBracket + Barrier*Review_educationBracket + CrossingSignal*Review_educationBracket + PedPed*Review_educationBracket + ScenarioType*Review_educationBracket + 
    NumberOfCharacters + Man + Woman + Pregnant + Stroller + OldMan + OldWoman + Boy + Girl + Homeless + LargeWoman + LargeMan + 
    Criminal + MaleExecutive + FemaleExecutive + FemaleAthlete + MaleAthlete + FemaleDoctor + MaleDoctor + Dog + 
    DescriptionShown + LeftHand + ScenarioOrder + Template 
  , cluster = "ResponseID",
  data = df
)
summary(m)

# GPT4 is more likely to make prediction errors (0=correct, 1=error) for older individuals, low education, and lower income. 
# The LLM is less likely to make prediction errors for more religious respondents but is more likely to wrongly predict responses of very religious individuals.
# These associations of errors with demographics are rather small, e.g. 3pp for Less than high school. 
# The continuous variables were rescaled by 2 standard deviations to make them comparable to dummy variables (Gelman trick). 




