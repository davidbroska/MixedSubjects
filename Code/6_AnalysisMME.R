# Load packages and custom functions
source("Code/RFunctions.R")

# Load data
gpt4t = read_csv(get_filepath("3_gpt4turbo_wp_20241118.csv.gz"))
gpt4o = read_csv(get_filepath("3_gpt4o_wp_20240603.csv.gz"))
gpt35 = read_csv(get_filepath("3_gpt35turbo0125_wp_20240603.csv.gz"))



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
  writeLines(con=paste0(get_filepath("Figures"),"/6_CorrelationTable.tex"))


###############################################################
# Compare AMCE estimate from PPI with WLS in Moral Machine data
###############################################################

# Load association between predicted and observed responses
rhos = read_csv("Data/5_rho.csv") 

label_rho = function(.label, .ppi_corr){
  
  # Format PPI correlation 
  ppi_corr = .ppi_corr %>% 
    round(3) %>% 
    as.character() %>% 
    sub("^0\\.", ".", .) %>% 
    ifelse(nchar(.) == 3, paste0(.,"0"), .)

  # Combine label and PPI correlation with math symbol
  combined = bquote(.(.label)~"("*tilde(rho)==.(ppi_corr)*")")
  
  return(combined)
}

# Example use 
label_rho("Variable", 0.5)



# Create legend for figures
colors = tribble(
  ~Variable,       ~Code,        ~Label,
  "Species",       "#FFBC79FF",  "Sparing humans vs animals",
  "Social Status", "#7B848FFF",  "Sparing high status vs low status",
  "Utilitarian",   "#A3CCE9FF",  "Sparing more characters vs fewer",
  "Age",           "#1170AAFF",  "Sparing the young vs old",
  "Gender",        "#5FA2CEFF",  "Sparing women vs men",
  "Fitness",       "#57606CFF",  "Sparing the fit vs the large",
  "CrossingSignal","#FC7D0BFF",  "Sparing the lawful vs unlawful",
  "Barrier",       "#C8D0D9FF",  "Sparing pedestrians vs passengers",
  "Intervention",  "#C85200FF",  "Preference for inaction vs intervention") %>% 
  left_join(rhos,  by = c("Variable" = "x")) %>% 
  arrange(ppi_corr) %>% 
  mutate(
    # Combine variable labels with ppi_corr
    Label = purrr::map2(.x = Label, .y = ppi_corr, .f = label_rho)
  )  


# Reorder factors for plotting 
fct_levels = colors %>% 
  arrange(ppi_corr) %>% 
  pull(Variable)

colors$Variable = factor(colors$Variable, levels = fct_levels)
  

# Load Variable# Load PPI estimates
dd = read_csv("Data/5_ResultsPPI_coord1.csv.gz") %>% 
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



#########################################
# Bias plots for PPI and silicon sampling
#########################################

# Maximum bias in silicon sampling
max_bias = db %>% 
  filter(method=="sil") %>% 
  slice(which.max(abs(bias)))

# Maximum bias as a percentage of the range of the scale of the dependent variable
round(100 * max_bias$bias/(1-0), 1)

# Ground truth AMCE
round(abs(max_bias$param), 2)

# Maximum bias as a percentage of the ground truth AMCE
round(100*abs(max_bias$bias)/abs(max_bias$param),0)

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
    x = "Number of silicon subjects N",
    y = "Bias", 
    color = "Scenario\nAttribute"
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    limits = c(min(db$bias), max(db$bias))
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code, 
    labels = colors$Label
  ) +
  guides(
    color = guide_legend(nrow = 3)
  ) +
  theme(
    legend.key.height = unit(0.25, "cm"), 
    panel.grid.major = element_line(linewidth = 0.2),  
    panel.grid.minor = element_line(linewidth = 0.1),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt"),
    legend.position = "bottom"
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
  scale_y_continuous(
    limits = c(min(db$bias), max(db$bias)),
  ) +
  labs(
    linetype = "Language Model", 
    y = "Bias",
    x = "Number of silicon subjects N"
  ) +
  scale_color_manual(
    breaks = colors$Variable, 
    values = colors$Code
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
  ggplot(aes(N, 100*ratio, color = x)) + 
  geom_line() +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
  ) +
  coord_cartesian(ylim = c(NA, 100)) + 
  guides(
    color = "none"
  ) +
  labs(
    x = "Number of silicon subjects N",
    y = "CI width as percentage of classical CI"
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
  

pp_ppi


# Precision of silicon sampling 
pp_sil = dp %>% 
  filter(method == "sil") %>% 
  ggplot(aes(N, 100*ratio, color = x)) + 
  geom_line() +
  labs(
    x = "Number of silicon subjects N",
    y = "CI width as percentage of classical CI",
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    breaks = seq(0,100,20),
  ) +
  coord_cartesian(ylim = c(0, 100)) + 
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

# ppi coverage plot
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
    x = "Number of silicon subjects N", 
    y="Percentage of CIs that cover parameter"
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

# silicon sampling coverage plot
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
    x = "Number of silicon subjects N", 
    y = "Percentage of CIs that cover parameter") +
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
titles = paste0(paste0(rep(" ", 14),collapse = ""), "Mixed subjects",
                paste0(rep(" ", 79),collapse = ""), "Silicon subjects")

# Combine plots
p = (pb_ppi+pb_sil) / (pp_ppi+pp_sil) / (pc_ppi+pc_sil) +
  plot_layout(
    nrow = 3,
    guides = "collect"
  ) &
  plot_annotation(
    title = titles,
    tag_levels = "a",
  ) &
  theme(
    legend.position = "bottom", 
    plot.title = element_text(hjust = .5, vjust = -3, size = 15),
    plot.margin = margin(t=4, r=8, b=2, l=4, "pt"),
    plot.tag = element_text(hjust=1, face = "bold")
  )

print(p)
ggsave(filename = paste0("Figures/6_SimulationResults.pdf"), 
       plot=p, width=11, height=11.7)




#######################
# Effective sample size
#######################

# Define function that calculates the effective sample size
n0 = function(rho, n, k) {
  # Define k as the ratio k = N/n
  
  # Effective sample size
  n0 = (n * (k+1)) / (k*(1-rho^2)+1) 
  
  return(n0)

}

# Compute effective sample size for N=100,000
n0_mme = dd %>% 
  filter(N == 10^5, n == 10^4) %>% 
  mutate(
    ppi_corr = round(ppi_corr, 3),
    n0 = n0(ppi_corr,n,N/n) %>% as.integer()
  ) %>% 
  arrange(desc(n0)) %>% 
  select(x, ppi_corr, n0) 



# Save LaTex table
n0_caption = paste0(
  "PPI correlations and effective sample sizes for each attribute in the Moral Machine experiment. ",
  "These statistics were computed for sample sizes n=10,000 and N=100,000 in our simulation."
)

n0_mme %>% 
  kable( 
    booktabs=F, escape = F,
    format = "latex",
    align = c("l","c","c"),
    label = "n0-mme",
    col.names = c("Attribute","PPI Correlation","Effective sample size"),
    caption = n0_caption,
  ) %>% 
  collapse_rows(columns = 1) %>% 
  kable_styling(latex_options = "hold_position") %>% 
  writeLines(con=paste0(get_filepath("Figures"),"/6_n0MME.tex"))



################################
# Scatterplot for MME simulation
################################

# Define custom colors and line types
custom_colors = c("Silicon Sampling" = "#1f78b4","Prediction-Powered Inference" = "#33a02c")
custom_shapes = c("Prediction-Powered Inference" = 16, "Silicon Sampling" = 15)


dscatter = dd %>% 
  filter(N == 10^5) %>% 
  select(param,beta_ppi, beta_sil) %>% 
  pivot_longer(
    cols = c(beta_ppi,beta_sil), 
    names_to = "estimator", 
    values_to = "estimate"
  ) %>%
  mutate(
    estimator = case_when(
      estimator == "beta_ppi" ~ "Prediction-Powered Inference",
      estimator == "beta_sil" ~ "Silicon Sampling",
      TRUE ~ estimator
    )
  ) 

pscatter = ggplot(dscatter, aes(param,estimate, color=estimator, shape=estimator)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 2) + 
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    x = "AMCE parameter",
    y = "AMCE estimate",
    color = "", 
    shape = ""
  ) +
  theme(legend.position = "bottom") +
  coord_fixed()

# Save scatterplot
ggsave(pscatter,filename=paste0(get_filepath("Figures"),"/5_ScatterplotAMCE.pdf"),width=5,height=5)



################################################################
# Prepare data from the American Community Survey for comparison 
################################################################

# Read data with survey responses and apply exclusion criteria from Awad et al
acs = get_filepath("usa_00004.csv.gz") %>% 
  fread() %>% 
  rename_with(~ str_remove(., "US2016C_")) %>% 
  filter(
    # Filter out Puerto Rico
    ST!=72,
    AGEP >= 15,
    AGEP <  95) %>% 
  mutate(
    # Correct income for years
    CorrPINCP = as.numeric(PINCP)*as.numeric(ADJINC)/1000000,
    
    IncomeBracketSmall = case_when(
      CorrPINCP<5000 ~"$0-$5,000",
      CorrPINCP>=5000 & CorrPINCP<25000 ~"$5,001-\n$25,000",
      CorrPINCP>=25000 & CorrPINCP<50000 ~"$25,001-\n$50,000",
      CorrPINCP>=50000 & CorrPINCP<100000 ~"$50,001-\n$100,000",
      CorrPINCP>=100000 ~"More than\n$100,000") %>% 
      factor(levels=c("$0-$5,000","$5,001-\n$25,000","$25,001-\n$50,000","$50,001-\n$100,000","More than\n$100,000")),
    
    AgeBracket = case_when(AGEP>=15 & AGEP<25 ~"15-24",
                           AGEP>=25 & AGEP<35 ~"25-34",
                           AGEP>=35 & AGEP<45 ~"35-44",
                           AGEP>=45 & AGEP<55 ~"45-54",
                           AGEP>=55 & AGEP<65 ~"55-64",
                           AGEP>=65 & AGEP<75 ~"65-74",
                           AGEP>=75 & AGEP<85 ~"75-84",
                           AGEP>=85 & AGEP<95 ~"85-94") %>% 
      factor(levels=c("15-24","25-34","35-44","45-54","55-64","65-74","75-84","85-94")),
    
    Gender = case_when(SEX==1 ~"Man", SEX==2 ~"Woman") %>% 
      factor(levels=c("Man","Woman")),
    
    EducationBracket:= case_when(
      SCHL<=15 ~ "Less than\nhigh school",
      SCHL %in% c(16:17) ~"High school",
      SCHL %in% c(18:21) ~"Some college",
      SCHL %in% c(22:24) ~"Postgraduate") %>% 
      factor(levels=c("Less than\nhigh school","High school","Some college","Postgraduate")))

## Calculate percentages and correct with weights
acsPerc = acs %>% 
  group_by(Gender,AgeBracket,IncomeBracketSmall,EducationBracket) %>% 
  summarise(ACSn = sum(PWGTP)) %>% 
  ungroup() %>% 
  mutate(ACSfreq = ACSn/sum(ACSn))

# Check that there are no missing values
summarize_all(acsPerc, ~ sum(is.na(.)))


################################################################################
# Compare demographic distribution of samples ----------------------------------
################################################################################

compute_dem_share = function(.acs_var,.mm_var){
  
  # American Community Survey
  acsvar = acs %>% 
    group_by({{.acs_var}}) %>% 
    summarize(n = sum(PWGTP)) %>% 
    ungroup() %>% 
    mutate(acsFreq = n / sum(n)) %>% 
    select(-n)
  
  # Moral Machine Sample
  mmsvar = gpt4t %>% 
    group_by({{.mm_var}}) %>% 
    summarize(n = length(unique(UserID))) %>% 
    ungroup() %>% 
    mutate(mmsFreq = n / sum(n)) %>% 
    select(-n)
  
  # Join
  acsvar %>% 
    left_join(mmsvar,by=join_by({{.acs_var}} == {{.mm_var}})) %>% 
    mutate(mmsFreq = mmsFreq %>% ifelse(is.na(.),0,.), 
           Variable = colnames(select(acs,{{.acs_var}}))) %>% 
    select(Variable, everything()) %>% 
    rename(Level = {{.acs_var}})
  
}

# Calculate frequencies of demographic categories for each of the datasets
GenderFreq = compute_dem_share(Gender,Review_gender)
AgeFreq = compute_dem_share(AgeBracket,Review_ageBracket)
EducationFreq = compute_dem_share(EducationBracket,Review_educationBracket) 
IncomeFreq = compute_dem_share(IncomeBracketSmall,IncomeBracketSmall) 

# Assemble statistics and compute absolute difference in relative frequencies
FreqWide = GenderFreq %>% 
  bind_rows(AgeFreq) %>% 
  bind_rows(EducationFreq) %>% 
  bind_rows(IncomeFreq) %>% 
  mutate(absDiffmms = abs(acsFreq - mmsFreq))

# Calculate mean absolute difference in percentage points per variable
DiffPP = FreqWide %>% 
  group_by(Variable) %>% 
  summarise(AvgAbsDiffMMS = mean(absDiffmms))
DiffPP

# Mean absolute difference across variables
mean(DiffPP$AvgAbsDiffMMS)


# Create bar plot with relative frequencies of demographics
cols = tribble(
  ~var,     ~col,      ~lab, 
  "mmsFreq",  "#CC6677", "\nMoral Machine U.S. Sample\n",
  "acsFreq",  "#66aacc", "\nAmerican Community Survey 2016\n") %>% 
  mutate(lab = str_replace_all(lab," ","\n"),
         var = factor(var,ordered = T))

# Create labels for plots
labell = c(AgeBracket="Age",EducationBracket="Education",
           Gender="Gender",IncomeBracketSmall="Income")

# Convert from wide to long data format
FreqLong = FreqWide %>% 
  pivot_longer(cols = c(acsFreq,mmsFreq)) %>%
  mutate(name = factor(name,levels=cols$var))

# Create ggplot 
DemPlot = FreqLong %>% 
  ggplot(aes(Level, value, fill=name)) +
  geom_col(position = position_dodge(), width=0.5) +
  facet_wrap(~ Variable, scales = "free_x",
             labeller = labeller(Variable=labell)) +
  scale_fill_manual(breaks = cols$var, values = cols$col,labels=cols$lab) +
  labs(fill = "Dataset",x="",y="Relative frequency") +
  theme(axis.text.x = element_text(size = 9.5))

DemPlot = FreqLong %>% 
  ggplot(aes(Level, value, fill = name)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~ Variable, scales = "free_x",
             labeller = labeller(Variable = labell)) +
  scale_fill_manual(
    breaks = cols$var, 
    values = cols$col, 
    labels = cols$lab
  ) +
  labs(
    fill = "Dataset", 
    x = "", 
    y = "Relative frequency"
  ) 
DemPlot

# Save plot
ggsave(DemPlot,filename=paste0(get_filepath("Figures"),"/6_DemographicDistribution.pdf"),width=9,height=6)
















