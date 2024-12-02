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
write_csv(main.Saved, paste0(get_filepath("Data"),"/3_AmceParamsSimulationR.csv.tar"))


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

ggsave(filename="Figures/3_AMCEs.pdf",width=7,height=5)



