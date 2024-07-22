# Load packages and custom functions
source("Code/1_Functions.R")

# Load data
df = read_csv("Data/5_SurveySampleLLM.csv.gz")



####################
# Summary statistics
####################

# Correlation between predicted and observed responses
corr_tab = df %>%
  pivot_longer(cols=matches(".Saved"), values_to="ModelSaved") %>% 
  group_by(name) %>% 
  summarize(
    correlation = cor(Saved,ModelSaved,use = "complete.obs"), 
    N = sum(!is.na(ModelSaved))/2) %>% 
  mutate(
    Type = case_when(
      str_detect(name, "wp_Saved$") ~  "Prediction", 
      str_detect(name, "wp_Saved_2") ~ "Replicate 1", 
      str_detect(name, "wp_Saved_3") ~ "Replicate 2", 
      str_detect(name, "np_Saved") ~ "Without persona",
      str_detect(name, "_mode") ~ "Mode across prediction and replicates") %>% 
      factor(levels = c("Prediction","Replicate 1","Replicate 2","Mode across prediction and replicates","Without persona")), 
    Model = case_when(
      str_detect(name, "gpt35turbo0125") ~ "GPT3.5 Turbo",
      str_detect(name, "gpt4o") ~ "GPT4o",
      str_detect(name, "gpt4turbo") ~ "GPT4 Turbo"
    ) %>% factor(levels = c("GPT4 Turbo","GPT4o","GPT3.5 Turbo"))) %>% 
  select(Model, Type, Correlation = correlation, N) %>% 
  arrange(Model, Type, desc(N))  



caption = paste0(
  "Pearson correlation of survey respondents' decision for a moral dilemma with the LLM predicted decision. ",
  "In addition to the 22,315 predictions, we assess how the correlation varies by prompting the LLM to give 5,000 additional predictions. ",
  "We form a composite from the modal prediction of three identical prompts. For a separate set of predictions, we omit the demographic persona from the prompt."
)
kable(corr_tab, format = "latex",digits = 3, label = "corr-tab",booktabs=T, align = c("l","l","c","c"),
      caption = caption) %>% 
  collapse_rows(columns = 1) %>% 
  kable_styling(latex_options = "hold_position") %>% 
  writeLines(con="Figures/6_CorrelationTable.tex")



# correlations of observed and predicted outcomes
round(cor(df[,c("Saved","gpt4turbo_wp_Saved","gpt4o_wp_Saved","gpt35turbo0125_wp_Saved")],use="complete.obs"),4)

# correlations of observed with modal prediction 
round(cor(df[,c("Saved","gpt4turbo_wp_Saved_mode","gpt4o_wp_Saved_mode","gpt35turbo0125_wp_Saved_mode")],use="complete.obs"),4)

reps = c("gpt4turbo_wp_Saved","gpt4turbo_wp_Saved_2","gpt4turbo_wp_Saved_3","gpt4turbo_wp_Saved_mode",
         "gpt35turbo0125_wp_Saved","gpt35turbo0125_wp_Saved_2","gpt35turbo0125_wp_Saved_3","gpt35turbo0125_wp_Saved_mode")
round(cor(df[,c("Saved",reps)],use = "complete.obs"),4)

# Function to compute correlation over prompts
cumulative_correlation <- function(x, y) {
  n <- length(x)
  cor_values <- numeric(n)
  
  for (i in 10:n) {
    cor_values[i] <- cor(x[1:i], y[1:i], use="complete.obs")
  }
  
  return(cor_values)
}

# Compute cumulative correlation
df$ccorr_35_wp = cumulative_correlation(df$gpt35turbo0125_wp_Saved, df$Saved)
df$ccorr_35_np = cumulative_correlation(df$gpt35turbo0125_np_Saved, df$Saved)

df$ccorr_4t_wp = cumulative_correlation(df$gpt4turbo_wp_Saved, df$Saved)
df$ccorr_4t_np = cumulative_correlation(df$gpt4turbo_np_Saved, df$Saved)

df$ccorr_4o_wp = cumulative_correlation(df$gpt4o_wp_Saved, df$Saved)
df$ccorr_4o_np = cumulative_correlation(df$gpt4o_np_Saved, df$Saved)

# Plot cumulative correlation
dvs = c("gpt35turbo0125_wp_Saved","gpt4o_wp_Saved","gpt4turbo_wp_Saved",
        "gpt35turbo0125_wp_Saved","gpt4o_wp_Saved","gpt4turbo_wp_Saved")
ccorrs = c("ccorr_35_wp","ccorr_35_np",
           "ccorr_4t_wp","ccorr_4t_np",
           "ccorr_4o_wp","ccorr_4o_np")

df %>% 
  pivot_longer(cols=all_of(ccorrs)) %>% 
  group_by(name) %>% 
  mutate(rownum = row_number()) %>% 
  filter(!is.na(value), rownum >=100) %>% 
  ggplot(aes(rownum/2,value,color = name)) +
  geom_line(linewidth=.3) +
  scale_y_continuous(breaks = seq(-1,1,0.1)) +
  labs(y="Cumulative Correlation",x="Number of prompts",color="Model")
ggsave("Figures/6_CumulativeCorrelation.pdf", width=6, height=5)








