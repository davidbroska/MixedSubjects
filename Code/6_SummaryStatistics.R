# Load packages and custom functions
source("Code/RFunctions.R")

# Load data
gpt4t = read_csv("Data/4_gpt4turbo_wp_20241118.csv.gz")
gpt4o = read_csv("Data/4_gpt4o_wp_20240603.csv.gz")
gpt35 = read_csv("Data/4_gpt35turbo0125_wp_20240603.csv.gz")
cor(gpt4t$gpt4turbo_wp_Saved, gpt4t$Saved)
  


#####################################################################
# Compute pearson correlation between predicted and observed outcomes
#####################################################################

pearson_corr = function(.df){
  
  .df %>% 
    pivot_longer(
      cols = matches(".Saved"), 
      names_to = "LLM", 
      values_to = "LLMSaved"
    ) %>% 
    mutate(across(matches("Saved"), as.numeric)) %>% 
    group_by(LLM) %>% 
    summarize(
      nrows = sum(!is.na(LLMSaved)),
      r_corr = cor(Saved,LLMSaved,use = "complete.obs")
    )
  }

# Compute correlations for each LLM
corr = bind_rows(
    pearson_corr(gpt4t),
    pearson_corr(gpt4o),
    pearson_corr(gpt35),
  ) %>% 
  mutate(
    LLM = case_when(
      LLM == "gpt4turbo_wp_Saved" ~ "gpt-4-turbo",
      LLM == "gpt4o_wp_Saved" ~ "gpt-4o",
      LLM == "gpt35turbo0125_wp_Saved" ~ "gpt-3.5-turbo-0125"),
    r_corr = round(r_corr, 3)
  ) %>% 
  arrange(desc(r_corr))

# Information about LLMs from OpenAI
llms = tibble(
  LLM = c("gpt-4-turbo", "gpt-4o", "gpt-3.5-turbo-0125"),
  Context_window = c("128,000 tokens", "128,000 tokens", "16,385 tokens"),
  Training_data = c("Up to Dec 2023", "Up to Oct 2023", "Up to Sep 2021"),
  Input_cost_1K_tokens = c(0.010, 0.005, 0.0005),
  Output_cost_1K_tokens = c(0.030, 0.015, 0.0015)
)

# Join the two datasets
corr_llms = left_join(llms, corr, by = "LLM")
corr_llms

# Print table to LaTex
kable(
    x = corr_llms, 
    booktabs=F, escape = F,
    format = "latex",
    align = c("l","c","c"),
    label = "corr-tab",
    col.names = c("LLM","N+n","Context\\\\ window","Training\\\\ data","Inputcost\\\\ 1K tokens","Output cost\\\\ 1K_tokens","Correlation"),
    caption = "Pearson correlation of survey respondents' decision for a moral dilemma with the LLM's predicted decision.",
  ) %>% 
  collapse_rows(columns = 1) %>% 
  kable_styling(latex_options = "hold_position") %>% 
  writeLines(con="Figures/6_CorrelationTable.tex")







###############################################################
# Compare AMCE estimate from PPI with WLS in Moral Machine Data
###############################################################

colors = tribble(
  ~Variable,       ~Code,        ~Label,
  "Species",       "#FFBC79FF",  "Sparing humans vs animals",
  "Utilitarian",   "#A3CCE9FF",  "Sparing more characters vs fewer",
  "Age",           "#1170AAFF",  "Sparing the young vs old",
  "Gender",        "#5FA2CEFF",  "Sparing women vs men",
  "CrossingSignal","#FC7D0BFF",  "Sparing the lawful vs unlawful",
  "Fitness",       "#57606CFF",  "Sparing the fit vs the large",
  "Barrier",       "#C8D0D9FF",  "Sparing pedestrians vs passengers",
  "Intervention",  "#C85200FF",  "Preference for inaction vs intervention",
  "Social Status", "#7B848FFF",  "Sparing high status vs low status"
)


# Load association between predicted and observed responses
rhos = read_csv("Data/7_rho.csv") 

# Load PPI estimates
dd = read_csv("Data/7_ResultsPPI.csv.gz") %>% 
  mutate(
    ratio_ppi_hum_se = se_ppi / se_hum,
    ratio_sil_hum_se = se_sil / se_hum,
  ) 

glimpse(dd)

# Check for NA values
summarise(dd, across(everything(), ~ sum(is.na(.))))

# bias
db = dd %>% 
  pivot_longer(
    cols = c(bias_ppi,bias_sil), 
    values_to="bias",
    names_to="method"
  ) %>% 
  mutate(method = method %>% str_extract("ppi|sil|hum")) 

# precision
dp = dd %>% 
  pivot_longer(
    cols = c(ratio_sil_hum_se,ratio_ppi_hum_se), 
    values_to="ratio",
    names_to="method") %>% 
  mutate(method = method %>% str_extract("ppi|sil|hum")) 

# coverage
dc = dd %>% 
  pivot_longer(
    cols = c(coverage_ppi,coverage_sil),
    values_to="coverage",
    names_to="method") %>% 
  mutate(method = method %>% str_extract("ppi|sil|hum")) 
  

brewer_palette = "Set2"

format_digit = function(.number){
  .number %>% 
    round(2) %>% 
    as.character() %>% 
    sub("^0\\.", ".", .) %>% 
    ifelse(nchar(.) == 2, paste0(.,"  "), .)
}

r = dp %>% 
  group_by(x,y) %>% 
  slice(which.max(N)) %>% 
  mutate(ppi_corr = format_digit(ppi_corr))

add_labs = function(.plot, .var, .ynudge=0){
  
  
  lab = deparse(bquote(" "*tilde(rho)==.(filter(r,x==.var)$ppi_corr)))
  
  y = r %>% 
    filter(x==.var) %>% 
    pull(ratio) %>%  
    {{.*100}}
  
  annotated = .plot + 
    annotate("text",label=lab,y=y+.ynudge, size=asize, 
             parse=T, x=xnudge,family = "serif") 
  
  return(annotated)
}


  
asize = 3.65
xnudge = 10^6/10^4 + .383


#########################################
# Bias plots for PPI and silicon sampling
#########################################

# Bias PPI 
pb_ppi = db %>% 
  filter(method == "ppi") %>% 
  ggplot(aes(N, bias, color = x)) + 
  geom_hline(
    yintercept = 0, 
    color = "darkgrey", 
    linetype = "dotted"
  ) +
  geom_line(
    linewidth = 0.4
  ) +
  labs(
    x = "Number of predictions for human subject N/n",
    y = "Bias", 
    color = "Scenario\nAttribute"
  ) +
  guides(
    color = guide_legend(nrow = 3)
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    legend.key.height = unit(0.25, "cm"), 
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt"),
    legend.position = "none"
  )
pb_ppi



# Bias silicon sampling
pb_sil = db %>% 
  filter(method == "sil") %>% 
  ggplot(aes(N, bias, color = x)) + 
  geom_hline(
    yintercept = 0, 
    color = "darkgrey", 
    linetype = "dotted"
  ) +
  geom_line(
    linewidth = 0.4
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  labs(
    linetype = "Language Model", 
    y = "Bias",
    color = "", 
    x = "Number of predictions for every human subject N/n"
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  ) 
pb_sil


#######################################
# Precision of PPI and silicon sampling
#######################################

# Precision of PPI 
pp_ppi = dp %>% 
  filter(method == "ppi") %>% 
  ggplot(aes(N, ratio, color = x)) + 
  geom_line() +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    breaks = seq(0,1,0.02), 
    limits = c(0.9,1),
  ) +
  guides(
    color = "none"
  ) +
  labs(
    x = "Number of predictions for every human subject N/n",
    y = "PPI CI width as percentage of classical CI",
    color=""
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  ) 
  


pp_ppi = pp_ppi %>% 
  add_labs("Species") %>% 
  add_labs("Utilitarian",.ynudge = 0.0005) %>%  
  add_labs("Age",.ynudge = -0.0005) %>% 
  add_labs("Gender", .ynudge = 0.00085) %>% 
  add_labs("CrossingSignal", .ynudge = -0.0012) %>% 
  add_labs("Fitness", .ynudge = 0.001) %>% 
  add_labs("Barrier",.ynudge = -0.0006) %>% 
  add_labs("Intervention")  

pp_ppi


# Precision of silicon sampling 
pp_sil = dp %>% 
  filter(method == "sil") %>% 
  ggplot(aes(N, 100*ratio, color = x)) + 
  geom_line() +
  labs(
    x = "Number of predictions for every human subject N/n",
    y = "PPI CI width as percentage of classical CI",
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    breaks = c(0,20,40,60,80,100),
    limits = c(0,100),
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  )
pp_sil

######################################
# Coverage of PPI and silicon sampling
######################################
pc_ppi = dc %>%
  filter(method == "ppi") %>% 
  ggplot(aes(N, 100*coverage, color=x)) + 
  geom_hline(
    yintercept = 95, 
    color = "darkgrey",
    linetype = "dotted"
  ) +
  geom_line() +
  labs(
    x = "Number of predictions for every human subject N/n", 
    y="% of CIs that cover parameter"
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    labels = label_comma(),
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1), 
    breaks = c(0,20,40,60,80,100),
    limits = c(0,100)
  ) + 
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2), 
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  )
pc_ppi
# coverage plot
pc_sil = dc %>%
  filter(method == "sil") %>% 
  ggplot(aes(N, 100*coverage, color=x)) + 
  geom_hline(
    yintercept = 95, 
    color = "darkgrey", 
    linetype = "dotted"
  ) +
  geom_line() +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1), 
    breaks = c(0,20,40,60,80,100),
    limits = c(0,100)
  ) + 
  labs(
    x = "Number of predictions for every human subject N/n", 
    y = "% of CIs that cover parameter") +
  guides(
    color = "none"
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  ) 

pc_sil




# Create titles for combined plot
titles = paste0(paste0(rep(" ", 12),collapse = ""), "Mixed subjects",
                paste0(rep(" ", 79),collapse = ""), "Silicon subjects")

# Combine plots
p = (pb_ppi+pb_sil) / (pp_ppi+pp_sil) / (pc_ppi+pc_sil) +
  plot_layout(
    nrow = 3,
    guides = "collect"
  ) &
  plot_annotation(
    title = titles
  ) &
  theme(
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5, vjust = 0, size = 15),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt")
  )

print(p)
ggsave(filename = paste0("Figures/8_SimulationResults.pdf"), 
       plot=p, width=11, height=11.7)











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



##############################################
# Calculate AMCE for human and LLM predictions
##############################################




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
main.gpt4turbo_wp_Saved = calculate_amce(mms,"gpt4turbo_wp_Saved_1")

# # AMCE for GPT 4o
# main.gpt4o_wp_Saved = calculate_amce(mms,"gpt4o_wp_Saved_1") 
# 
# # AMCE for GPT3.5 Turbo
# main.gpt35turbo0125_wp_Saved = calculate_amce(mms,"gpt35turbo0125_wp_Saved_1") 

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
  "gpt4turbo_wp_Saved_1",      "#4477AA", "Silicon Sampling\n(GPT-4 Turbo) ",
  "Saved",                   "#DDCC77", " \nMoral Machine\nSample\n ",
  "Awad2018",                "#CC6677", " \nAwad et al.\n(2018)\n "
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
  theme(
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text = element_text(size = 9))

ggsave(filename="Figures/8_AMCEs.pdf",width=7,height=5)







