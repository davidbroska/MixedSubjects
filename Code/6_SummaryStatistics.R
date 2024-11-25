# Load packages and custom functions
source("Code/1_Functions.R")

# Load data
df = read_csv("Data/5_SurveySampleLLM.csv.gz") 



# if dem, then take frist row and cluster on userid 

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
    Type = name %>% 
      str_replace(".*wp_Saved_", "Prediction ") %>% 
      str_replace(".*wp_Saved_", "Prediction ") %>% 
      str_replace(".*wp_Saved_", "Prediction ") %>% 
      str_replace(".*np_Saved_", "Without persona ") , 
    Model = case_when(
      str_detect(name, "gpt35turbo0125") ~ "GPT3.5 Turbo",
      str_detect(name, "gpt4o") ~ "GPT4o",
      str_detect(name, "gpt4turbo") ~ "GPT4 Turbo",
      str_detect(name, "o1mini") ~ "o1 Mini",
      str_detect(name, "o1preview") ~ "o1 Preview",
      str_detect(name, "claude35sonnet20241022") ~ "Claude 3.5 Sonnet",
      str_detect(name, "all_wp_Saved_mode") ~ "All models with persona") %>% 
      factor(levels = c("o1 Mini","o1 Preview","GPT4 Turbo","GPT4o","GPT3.5 Turbo","Claude 3.5 Sonnet", "All models with persona"))) %>% 
  select(Model, Type, Correlation = correlation, N) %>% 
  arrange(Model, Type, desc(N))  

print(corr_tab,n=25)


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

corr_tab


# correlations of observed and predicted outcomes
# round(cor(df[,c("Saved","gpt4turbo_wp_Saved_1","gpt4o_wp_Saved_1","gpt35turbo0125_wp_Saved_1")],use="complete.obs"),4)
# 
# # correlations of observed with modal prediction 
# round(cor(df[,c("Saved","gpt4turbo_wp_Saved_mode","gpt4o_wp_Saved_mode","gpt35turbo0125_wp_Saved_mode")],use="complete.obs"),4)
# 
# reps = c("gpt4turbo_wp_Saved_1","gpt4turbo_wp_Saved_2","gpt4turbo_wp_Saved_3","gpt4turbo_wp_Saved_mode",
#          "gpt35turbo0125_wp_Saved_1","gpt35turbo0125_wp_Saved_2","gpt35turbo0125_wp_Saved_3","gpt35turbo0125_wp_Saved_mode")
# round(cor(df[,c("Saved",reps)],use = "complete.obs"),4)


# # Function to compute correlation over prompts
# cumulative_correlation <- function(x, y) {
#   n <- length(x)
#   cor_values <- numeric(n)
#   
#   for (i in 10:n) {
#     cor_values[i] <- cor(x[1:i], y[1:i], use="complete.obs")
#   }
#   
#   return(cor_values)
# }
# 
# # Compute cumulative correlation
# df$ccorr_35_wp = cumulative_correlation(df$gpt35turbo0125_wp_Saved_1, df$Saved)
# 
# df$ccorr_4t_wp = cumulative_correlation(df$gpt4turbo_wp_Saved_1, df$Saved)
# 
# df$ccorr_4o_wp = cumulative_correlation(df$gpt4o_wp_Saved_1, df$Saved)
# 
# df$ccorr_o1mini_wp = cumulative_correlation(df$o1mini_wp_Saved_1, df$Saved)
# 
# df$ccorr_35sonnet_wp = cumulative_correlation(df$claude35sonnet20241022_wp_Saved_1, df$Saved)
# 
# 
# # Plot cumulative correlation
# dvs = c("gpt35turbo0125_wp_Saved_1","gpt4o_wp_Saved_1","gpt4turbo_wp_Saved_1","o1mini_wp_Saved_1","claude35sonnet20241022_wp_Saved_1")
# ccorrs = c("ccorr_35_wp","ccorr_4t_wp","ccorr_4o_wp","ccorr_o1mini_wp","ccorr_35sonnet_wp")
# 
# df %>% 
#   pivot_longer(cols=all_of(ccorrs)) %>% 
#   group_by(name) %>% 
#   mutate(rownum = row_number()) %>% 
#   filter(!is.na(value), rownum >=100, rownum <=1000) %>% 
#   ggplot(aes(rownum/2,value,color = name)) +
#   geom_line(linewidth=.3) +
#   scale_y_continuous(breaks = seq(-1,1,0.1)) +
#   labs(y="Cumulative Correlation",x="Number of prompts",color="Model")
# ggsave("Figures/6_CumulativeCorrelation.pdf", width=6, height=5)










