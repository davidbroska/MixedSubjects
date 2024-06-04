# Load functions
source("Code/1_Functions.R")

# Load sample from moral machine data
mms = read_csv("https://raw.githubusercontent.com/davidbroska/IntegrativeExperimentsGAI/main/Data/2_SurveySample.csv.gz") %>% 
  mutate_all(as.character)

# Load files with API output
files = c("gpt-3.5-turbo-0125_20240603.csv","gpt-4o_20240603.csv","gpt-4-turbo_20240603.csv")


# Aggregate files
df = files %>% 
  lapply(., function(file){
    
    writeLines(file)
    
    paste0("Data/",file) %>% 
      read_csv(show_col_types=F) %>% 
      mutate_all(as.character) %>% 
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
