# Load functions
source("Code/1_Functions.R")

# Load sample from moral machine data
mms = read_csv("https://raw.githubusercontent.com/davidbroska/IntegrativeExperimentsGAI/main/Data/2_SurveySample.csv.gz") %>% 
  mutate(across(everything(), as.character))

# Load files with API output
files = c("4_gpt-3.5-turbo-0125_wp_20240603.csv.gz","4_gpt-4o_wp_20240603.csv.gz","4_gpt-4-turbo_wp_20240603.csv.gz",
          "4_gpt-3.5-turbo-0125_np_20240603.csv.gz","4_gpt-4o_np_20240603.csv.gz","4_gpt-4-turbo_np_20240603.csv.gz")


# Aggregate files
df = files %>% 
  lapply(., function(file){
    
    writeLines(file)
    
    paste0("Data/",file) %>% 
      read_csv(show_col_types=F) %>% 
      mutate(across(everything(), as.character)) %>% 
      select(ExtendedSessionID:Cat, matches(".Saved"))
  
  }) %>% 
  purrr::reduce(full_join, by=colnames(mms)) %>% 
  full_join(mms, ., by=colnames(mms)) %>% 
  filter(!is.na(gpt35turbo0125_wp_Saved) | !is.na(gpt4turbo_wp_Saved) | !is.na(gpt4o_wp_Saved))

# Calculate weights for conjoint analysis 
df$weights = calcWeightsTheoretical(df)

# Check for NA values
summarise_all(df, ~sum(is.na(.))) %>% select_if(~any(.>0))

# Save file with predictions from LLMs
write_csv(df,"Data/5_SurveySampleLLM.csv.gz")
