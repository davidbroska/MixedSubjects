# Load packages and custom functions
source("Code/1_Functions.R")

# Load data
df = read_csv("https://raw.githubusercontent.com/davidbroska/IntegrativeExperimentsGAI/main/Data/5_SurveySampleLLM.csv.gz")
df = read_csv("Data/5_SurveySampleLLM.csv.gz")

# Convert from wide to long data format
dfl = df %>% pivot_longer(cols=matches(".Saved"), values_to="ModelSaved")


####################
# Summary statistics
####################

# Correlation between predicted and observed repsonses
dfl %>% 
  filter(!is.na(ModelSaved)) %>% 
  group_by(name) %>% 
  summarize(correlation = cor(Saved,ModelSaved), 
            n_prompts = length(unique(ResponseID)))

# Function to compute correlation over prompts
cumulative_correlation <- function(x, y) {
  n <- length(x)
  cor_values <- numeric(n)
  
  for (i in 10:n) {
    cor_values[i] <- cor(x[1:i], y[1:i])
  }
  
  return(cor_values)
}

# Compute cumulative correlation
df$ccorr_35_wp = cumulative_correlation(df$gpt35turbo0125_wp_Saved, df$Saved)
df$ccorr_4t_wp = cumulative_correlation(df$gpt4turbo_wp_Saved, df$Saved)
df$ccorr_4o_wp = cumulative_correlation(df$gpt4o_wp_Saved, df$Saved)

# Plot cumulative correlation
dvs = c("gpt35turbo0125_wp_Saved","gpt4turbo_wp_Saved","gpt4o_wp_Saved")
df %>% 
  pivot_longer(cols=c("ccorr_35_wp","ccorr_4t_wp","ccorr_4o_wp")) %>% 
  group_by(name) %>% 
  mutate(rownum = row_number()) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(rownum/2,value,color = name)) +
  geom_line() +
  scale_y_continuous(breaks = seq(-1,1,0.1)) +
  labs(y="Cumulative Correlation",x="Number of prompts",color="Model")
ggsave("Figures/7_CumulativeCorrelation.pdf", width=6, height=5)





